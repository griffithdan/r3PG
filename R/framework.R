# -*- coding: utf-8 -*-

## from ConfigParser import SafeConfigParser

##class BookKepper(object):
    ##"""a book keeper class, for output calculation results at each step"""

##    def __init__(self, fpath):
##        super(BookKepper, self).__init__()
##        self.fpath = fpath

##    def open(self):
##        self.handler = open(self.fpath, 'w+')

##    def shutdown(self):
##        if self.handler:
##            self.handler.close()

##    def write(self, message):
##        self.handler.write(message)

##initialize <- function(self, config){
##        self.list_out = []
##        for name in dir(config):
##            if not name.startswith('_'):
##                if getattr(config, name) == '1':
##                    self.list_out.append(name)
##        self.open()
##
##        self.handler.write('\t'.join(self.list_out) + '\n')
##
##
##    def keep(self, mapper, env):
##        v_out = [env[mapper[name]] for name in self.list_out]
##        message = '\t'.join(str(v) for v in v_out)
##        self.write('%s\n' % message)
##}

##class Model(object):
##    def __init__(self, fpath_config):
##        super(Model, self).__init__()
##
##        self.fpath_config = fpath_config
##        self.config = load_config(fpath_config)
##
##        self.data = None
##        self.bookeeper = None

##    def initialize(self):
##        raise Exception('Not Impelemnted')
##
##    def teardown(self):
##        raise Exception('Not Impelemnted')

##    def run(self):
##        raise Exception('Not Impelemnted')


##class Empty(object):
##    pass

load_config <- function(fpath_cfg){
    res <- read.ini(fpath_cfg)
    return(res)
}

## if __name__ == '__main__':
##     fpath_test = r'/Users/christopherstill/still/OSU/forestry/FES_599_Winter_2014/Py3PG/test/Test_config.cfg'
##     m = Model(fpath_test)
##     # print m.config.IO.input
##     # print m.config.IO.output
##     # print m.config.get('IO', 'input')
##     print m.config.Output
##     for name in dir(m.config.Output):
##         if not name.startswith('_'):
##             print name, getattr(m.config.Output, name)
