#!/usr/bin/env python

import os
import unittest

from subprocess import call

exe     = "/home/jimmisbl/Desktop/llac/src/parser/llama"
faildir = '/home/jimmisbl/Desktop/llac/testsuite/should_fail/'
passdir = '/home/jimmisbl/Desktop/llac/testsuite/should_compile/'

class ParametrizedTestCase(unittest.TestCase):
    """ Any TestCase classes that want to be parametrized 
        should inherit from this class.
    """
    def __init__(self, methodName='runTest', param=None):
        super(ParametrizedTestCase, self).__init__(methodName)
        self.param = param

    @staticmethod
    def parametrize(testcase_class, param=None):
        """ Create a suite containing all tests taken from the given
            subclass, passing them the parameter 'param'.
        """
        test_loader = unittest.TestLoader()
        test_names = test_loader.getTestCaseNames(testcase_class)
        suite = unittest.TestSuite()
        for name in test_names:
            suite.addTest(testcase_class(name, param))
        return suite

class ShouldFail(ParametrizedTestCase):
    def test_f(self):
        res = call([exe, faildir + "/" + self.param], stderr=err)
        try:
            self.assertEqual(res, 0)
        except AssertionError:
            print 'failed', self.param

class ShouldCompile(ParametrizedTestCase):
    def test_c(self):
        res = call([exe, passdir + "/" + self.param], stderr=err)
        try:
            self.assertEqual(res, 0)
        except AssertionError:
            print 'failed', self.param

if __name__ == '__main__':
    err = open("err.txt", 'w+r')
    suite = unittest.TestSuite()
    ffail = os.listdir(faildir)
    fpass = os.listdir(passdir)
    map(lambda x: suite.addTest(ParametrizedTestCase.parametrize(ShouldFail, x)),
            ffail)
    map(lambda x: suite.addTest(ParametrizedTestCase.parametrize(ShouldCompile, x)),
            fpass)
    unittest.TextTestRunner(descriptions=2, verbosity=2).run(suite)
    os.remove("err.txt")
