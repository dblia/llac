#!/usr/bin/env python

import os
import logging
import unittest

from subprocess import call

# FIXME: Replace hardcoded paths
exe  = "~/llac/main.native"
fdir = '~/llac/testsuite/up_to_semantic/should_fail/'
pdir = '~/llac/testsuite/up_to_semantic/should_compile/'

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
        res = call([exe, fdir + "/" + self.param], stderr=err, stdout=err)
        try:
            self.assertEqual(res, 1)
        except AssertionError:
            print 'ok', self.param

class ShouldCompile(ParametrizedTestCase):
    def test_c(self):
        res = call([exe, pdir + "/" + self.param], stderr=err, stdout=err)
        try:
            self.assertEqual(res, 0)
        except AssertionError:
            print 'failed', self.param

if __name__ == '__main__':
    err = open(os.devnull, 'w')
    suite = unittest.TestSuite()
    ffail = os.listdir(fdir)
    fpass = os.listdir(pdir)
    print "Semantic check"
    [suite.addTest(ParametrizedTestCase.parametrize(ShouldFail, x)) for x in ffail]
    [suite.addTest(ParametrizedTestCase.parametrize(ShouldCompile, x)) for x in fpass]
    unittest.TextTestRunner(verbosity=2).run(suite)

