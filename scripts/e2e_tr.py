import glob
import logging
import os
import subprocess

logging.basicConfig(level=logging.DEBUG)
log = logging.getLogger(__name__)

# End-to-end test runner for the vei compiler

TEST_DIR_PATH = './src/main/resources/test/marmoset/a5/'
ASM_EXEC_PATH = './scripts/nasm'
VEI_EXEC_PATH = './joosc'
OUTPUT_DIR = './output'


def assemble_file(asm_file):
    args = [ASM_EXEC_PATH, '-O1', '-f', 'elf', '-g', '-F', 'dwarf', asm_file]
    log.info('{}'.format(' '.join(args)))
    subprocess.call(args)


def link_files(output_dir):
    args = ['ld', '-melf_i386', '-o', 'main', '{}/*.o'.format(output_dir)]
    subprocess.call(args)


class SrcFile:
    def __init__(self, name, src):
        self.name = name
        self.src = src


class TestSuite:
    def __init__(self, name, files=[]):
        self.name = name
        self.file_names = files
        self.src_files = self._load_src_files(self.file_names)

    def _load_src_files(self, file_names):
        files = []
        for file_name in file_names:
            with open(file_name, 'r') as f:
                src = f.read()
                files.append(SrcFile(file_name, src))
        return files

    def compile(self):
        args = [VEI_EXEC_PATH]
        log.info(' '.join(args))
        subprocess.call(args)

    def __repr__(self):
        return '<TestSuite name={}, num_tests={}>'.format(self.name, len(self.src_files))


def gather_directory_tests(dir_path):
    test_files = glob.glob('{}/**/*.java'.format(dir_path), recursive=True)
    print(test_files)
    return test_files

def gather_tests(test_dir):
    files = os.listdir(test_dir)
    tests = []
    for f in files:
        file_path = os.path.join(test_dir, f)
        if os.path.isfile(file_path) and f.endswith('.java'):
            suite = TestSuite(f, [file_path])
            tests.append(suite)
        elif os.path.isdir(file_path):
            test_files = gather_directory_tests(file_path)
            suite = TestSuite(f, test_files)
            tests.append(suite)
        else:
            log.warn('got unexpected file in test directory {}'.format(file_path))
    print(tests)
    return tests
    # print(files)

if __name__ == '__main__':
    gather_tests(TEST_DIR_PATH)
    # assemble('filename.s')
