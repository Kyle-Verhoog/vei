FAILURE_MSGS = {
    'Compiler failed to detect error',
    'Compiler reported error in correct program'
}


class MarmoTestResult():
    ttype = None
    num = None
    outcome = None
    points = None
    name = None
    result = None

    def __init__(self, line):
        split = line.split()
        try:
            # <public|secret> <num> <outcome> <name> <name passed|failed> <?err msg>
            self.ttype = split[0]
            self.num = split[1]
            self.outcome = split[2]
            self.points = split[3]
            self.name = split[4]
            assert split[5] == self.name
            assert split[6] == self.outcome
            self.result = split[7]
        except Exception:
            pass

    @property
    def expected(self):
        return self.expected_result()

    def expected_result(self):
        if self.result is None:
            return '????'
        if 'error in correct program' in self.result:
            return 'pass'
        if 'failed to detect error' in self.result:
            return 'fail'
        return '????'


# sanity check to make sure data is formatted roughly correctly
def check_schema(line):
    pass
    #assert(line.split())


def parse_results(raw_results):
    # strip out the new lines and whitespace
    lines = [line.strip() for line in raw_results if line.strip()]

    # print(lines[0])
    # ttype, test_num, outcome, points, name, short, result, long_result = line

    results = []

    # combine test failure messages with the line they belong
    for i, line in enumerate(lines[1:]):
        if line in FAILURE_MSGS:
            prev_result = results[len(results)-1]
            prev_result.result = line
            continue
        results.append(MarmoTestResult(line))

    return results
