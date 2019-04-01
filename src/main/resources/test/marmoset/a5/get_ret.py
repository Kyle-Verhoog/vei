import glob
import os
import subprocess
import sys


JAVA_SRC = """
public class Runner {{
  public static void main(String[] args) {{
    java.lang.System.exit({}.test());
  }}
}}
"""

for f in os.listdir(os.getcwd()):
    if os.path.isdir(f):
        test_name = f
        print(test_name)
        files = glob.glob('{}/**/*.java'.format(f), recursive=True)
        with open('Runner.java', 'w') as runner:
            src = JAVA_SRC.format(test_name)
            runner.write(src)
    elif f.endswith('.java'):
        test_name = f[:-5]
        with open('Runner.java', 'w') as runner:
            src = JAVA_SRC.format(test_name)
            runner.write(src)
    else:
        test_name = ""
        continue

    print('processing {}'.format(test_name))
    args = ['/usr/lib/jvm/java-10-openjdk/bin/javac', 'Runner.java']
    r = subprocess.call(args)

    args = ['java', 'Runner']
    p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    ret = p.returncode

    print(stdout)
    print(stderr)
    print(ret)

    with open('{}.out'.format(test_name), 'wb') as outf:
       outf.write(stdout)
    with open('{}.ret'.format(test_name), 'w') as retf:
       retf.write(str(ret))


    subprocess.call(['rm', 'Runner.java'])
