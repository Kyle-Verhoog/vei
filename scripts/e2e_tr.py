from datetime import datetime
import glob
import logging
import os
import subprocess
import sys

logging.basicConfig(level=logging.INFO)
log = logging.getLogger(__name__)

# End-to-end test runner for the vei compiler

# TEST_DIR_PATH = './src/main/resources/test/marmoset/a5/'
TEST_DIR_PATH = 'src/main/resources/test/codegen/'
STDLIB_DIR_PATH = 'src/main/resources/test/marmoset/lib/5/'
ASM_EXEC_PATH = 'bin/nasm'
VEI_EXEC_PATH = './joosc'
OUTPUT_DIR = 'output/'


class SrcFile:
    def __init__(self, name, src):
        self.name = name
        self.src = src

    def __repr__(self):
        return '<SrcFile name={}>'.format(self.name)


class TestSuite:
    def __init__(self, name, files=[], stdlib=[]):
        self.name = name
        self._file_names = files
        self.src_files = self._load_src_files(self._file_names)
        self._stdlib_file_names = stdlib

    def _load_src_files(self, file_names):
        files = []
        main_test_file = None
        for file_name in file_names:
            with open(file_name, 'r') as f:
                src = f.read()
                src_file = SrcFile(file_name, src)
                if 'public static int test' in src:
                    if main_test_file:
                        log.error('multiple main test file for {}'.format(self))
                    else:
                        log.debug('set main test file {} for {}'.format(src_file, self.name))
                        main_test_file = src_file
                else:
                    files.append(src_file)

        # place the main test file first in the list of src files
        files.insert(0, main_test_file)
        return files

    def __len__(self):
        return len(self.src_files)

    def log_info(self, msg):
        log.info('{}({}): {}'.format(self.name, len(self), msg))

    @property
    def expected_status(self):
        if 'J1e' in self.name:
            return 42
        else:
            return 0

    @property
    def file_names(self):
        return [f.name for f in self.src_files]

    @property
    def stdlib(self):
        return self._stdlib_file_names

    @property
    def testname(self):
        return self.name[0:-5]

    def store_output(self, stdout, stderr):
        t = datetime.now().strftime("%Y%m%d-%H%M%S")
        stdout_log_file_name = '/tmp/{}_{}_stdout'.format(self.testname, t)
        stderr_log_file_name = '/tmp/{}_{}_stderr'.format(self.testname, t)
        self.log_info('output dumped to {}'.format(stdout_log_file_name))
        with open(stdout_log_file_name, 'wb') as f:
            f.write(stdout)
        self.log_info('output dumped to {}'.format(stderr_log_file_name))
        with open(stderr_log_file_name, 'wb') as f:
            f.write(stderr)

    def compile(self):
        args = [VEI_EXEC_PATH]
        args += self.file_names
        args += self.stdlib
        self.log_info(' '.join(args))
        p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        ret = p.returncode
        self.log_info('exit status {} (expected {})'.format(ret, self.expected_status))
        passed = self.expected_status == ret
        self.log_info('{}'.format('PASS' if passed else 'FAIL'))
        if not passed:
            self.store_output(stdout, stderr)
            sys.exit(-1)
        else:
            self.store_output(stdout, stderr)

    def assemble(self):
        asm_files = glob.glob('{}/**/*.s'.format(OUTPUT_DIR), recursive=True)
        for asm_file in asm_files:
            args = [ASM_EXEC_PATH, '-O1', '-f', 'elf', '-g', '-F', 'dwarf', asm_file]
            log.info('{}'.format(' '.join(args)))
            subprocess.call(args)

    def link(self):
        # out_files = glob.glob('{}/**/*.o'.format(OUTPUT_DIR), recursive=True)
        out_files = os.path.join(OUTPUT_DIR, '*.o')
        out_main = os.path.join(OUTPUT_DIR, 'main')
        args = 'ld -melf_i386 -o {} {}*.o'.format(out_main, OUTPUT_DIR)
        # log.info('{}'.format(' '.join(args)))
        log.info(args)
        subprocess.call(args, shell=True, env=os.environ.copy())

    def run(self):
        args = [os.path.join(OUTPUT_DIR, 'main')]
        log.info('{}'.format(' '.join(args)))
        p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        ret = p.returncode
        self.log_info('return code {}'.format(ret))

    def compile_and_run(self):
        self.compile()
        self.assemble()
        self.link()
        self.run()

    def __repr__(self):
        return '<TestSuite name={}, num_tests={}>'.format(self.name, len(self.src_files))


def gather_directory_files(dir_path):
    test_files = glob.glob('{}/**/*.java'.format(dir_path), recursive=True)
    return test_files


def gather_tests(test_dir):
    files = os.listdir(test_dir)
    stdlib_files = gather_directory_files(STDLIB_DIR_PATH)
    tests = []
    for f in files:
        file_path = os.path.join(test_dir, f)
        if os.path.isfile(file_path) and f.endswith('.java'):
            suite = TestSuite(f, [file_path], stdlib_files)
            tests.append(suite)
        elif os.path.isdir(file_path):
            test_files = gather_directory_files(file_path)
            suite = TestSuite(f, test_files, stdlib_files)
            tests.append(suite)
        else:
            log.warn('got unexpected file in test directory {}'.format(file_path))
    return tests


if __name__ == '__main__':
    # subprocess.call(['make'])
    # subprocess.call(['rm'])
    tests = gather_tests(TEST_DIR_PATH)
    # print('\n'.join([str(test) for test in tests]))
    test = tests[0]
    print(test)
    test.compile_and_run()
    # assemble('filename.s')
