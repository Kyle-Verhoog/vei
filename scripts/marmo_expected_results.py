#!/usr/bin/env python3
import sys
from parse_marmo import parse_results


if __name__ == '__main__':
    marmo_results_f = sys.argv[1]

    with open(marmo_results_f) as f:
        results = parse_results(f.readlines())

    # filter out tests that have passed, they don't give any information
    for result in results:
        print(result.name, result.expected)
