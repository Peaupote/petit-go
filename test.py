## A basic unit test script

import os, sys, subprocess

print(
"""
    ____       __  _ __     ______         ______          __
   / __ \___  / /_(_) /_   / ____/___     /_  __/__  _____/ /____  _____
  / /_/ / _ \/ __/ / __/  / / __/ __ \     / / / _ \/ ___/ __/ _ \/ ___/
 / ____/  __/ /_/ / /_   / /_/ / /_/ /    / / /  __(__  ) /_/  __/ /
/_/    \___/\__/_/\__/   \____/\____/    /_/  \___/____/\__/\___/_/

"""
)

HEADER    = '\033[95m'
OKBLUE    = '\033[94m'
OKGREEN   = '\033[92m'
WARNING   = '\033[93m'
FAIL      = '\033[91m'
ENDC      = '\033[0m'
BOLD      = '\033[1m'
UNDERLINE = '\033[4m'

FNULL = open(os.devnull, 'w')

exe = '_build/default/src/main.exe'
passed, tot = 0, 0

def test_goods(path):
    global passed, tot

    print("> {}Start new serie of tests{}".format(BOLD, ENDC))
    print("    {}\n".format(path))

    tests = os.listdir(path)
    tot += len(tests)
    tests.sort()

    align = max(map(len, tests))

    for test in tests:
        try:
            out = subprocess \
                .check_output([exe, "{}{}".format(path, test)],
                              stderr=FNULL) \
                .decode('utf-8')
            passed += 1

            print("{}[✓]{} {:<{a}}: Passed."
                  .format(OKGREEN, ENDC, test, a=align))
        except Exception as e:
            print("{}[x]{} {:<{a}}: Failed."
                  .format(FAIL, ENDC, test, a=align))

    print()

def test_bad(path):
    global passed, tot

    print("> {}Start new series of tests{}".format(BOLD, ENDC))
    print("    {}\n".format(path))

    tests = os.listdir(path)
    tot += len(tests)
    tests.sort()

    align = max(map(len, tests))

    for test in tests:
        try:
            out = subprocess \
                .check_output([exe, "{}{}".format(path, test)],
                              stderr=FNULL) \
                .decode('utf-8')

            print("{}[x]{} {:<{a}}: Failed."
                  .format(FAIL, ENDC, test, a=align))
        except Exception as e:
            passed += 1
            print("{}[✓]{} {:<{a}}: Passed."
                  .format(OKGREEN, ENDC, test, a=align))

    print()

test_bad('tests/syntax/bad/')
test_goods('tests/syntax/good/')

# test_bad('tests/typing/bad')
# test_goods('tests/typing/good')

print("> {}/{} tests passed.".format(passed, tot))
