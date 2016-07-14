from time import sleep
from json import loads, dumps

class dummy:
    pass

class query( dummy ):
    pass

class form( dummy ):
    pass

class report( dummy ):
    pass

class script( dummy ):
    pass

class baze:
    def __init__( self, infile, outfile, queries, forms, reports, scripts ):
        self.queries = {}
        self.forms = {}
        self.reports = {}
        self.scripts = {}
        
        self.infile = infile
        self.outfile = outfile
        
        for q, v in queries.items():
            self.queries[ q ] = query()
            self.queries[ q ].type = v[ 'type' ]
            self.queries[ q ].query = v[ 'query' ]
            self.queries[ q ].name = v[ 'name' ]
            def run( query ):
                return lambda: self.runQuery( query )
            self.queries[ q ].run = run( q )
        
        for f, v in forms.items():
            self.forms[ f ] = form()
            self.forms[ f ].name = v[ 'name' ]
            def show( form ):
                return lambda: self.showForm( form )
            self.forms[ f ].show = show( f )
        
        for r, v in reports.items():
            self.reports[ r ] = report()
            self.reports[ r ].name = v[ 'name' ]
            def show( report ):
                return lambda: self.showReport( report )
            self.reports[ r ].show = show( r )
        
        for k, v in scripts.items():
            self.scripts[ k ] = script()
            self.scripts[ k ].type = v[ 'type' ]
            self.scripts[ k ].script = v[ 'script' ]
            self.scripts[ k ].name = v[ 'name' ]
            def run( script ):
                return lambda: self.runScript( script )
            self.scripts[ k ].run = run( k )
    
    def getResult( self, rid ):
        found = False
        result = ''
        while True:
            fh = open( self.infile )
            r = fh.read()
            fh.close()
            js = loads( r )
            if rid in js.keys():
                found = True
                break
            else:
                sleep( 0.1 )
        result = js[ rid ]
        fh = open( self.infile, 'w' )
        del js[ rid ]
        fh.write( dumps( js ) )
        fh.close()
        return result
    
    def sendRequest( self, typ, oid, command=None, params=None ):
        # command for eventual SQL or similar commands to be executed by The Baze
        # params for eventual params when parametrized queries are implemented
        request = {}
        request[ "type" ] = typ
        request[ "oid" ] = oid
        if command:
            request[ "command" ] = command
        rq = dumps( request )
        fh = open( self.outfile, "w" )
        fh.write( rq )
        fh.close()
    
    def runQuery( self, query ):
        assert query in self.queries.keys()
        self.sendRequest( 'runquery', query )
        return self.getResult( query )
    
    def showForm( self, form ):
        assert form in self.forms.keys()
        self.sendRequest( 'showform', form)
        return self.getResult( form )
    
    def showReport( self, report ):
        assert report in self.reports.keys()
        self.sendRequest( 'showreport', report )
        return self.getResult( report )
    
    def runScript( self, script ):
        assert script in self.scripts.keys()
        self.sendRequest( 'runscript', script )
        return self.getResult( script )
            
        
        


if __name__ == '__main__':
    dbaze = baze( 
        'scriptin.json',
        'scriptout.json', 
        { 
            'query 1':{ 'name':'query 1', 'query':'select * from a', 'type':'SQL' }, 
            'query 2':{ 'name':'query 2', 'query':'select * from a', 'type':'SQL' }
        }, 
        { 'frmA':{ 'name':'frmA' }, 'frmB':{ 'name':'frmB' } }, 
        { 'rprtX':{ 'name':'rprtX' }, 'rprtY':{ 'name':'rprtY' }, 'rprtZ':{ 'name':'rprtZ' } }, 
        { 'my r script':{ 'name':'my r script', 'type':'R', 'script':'print(1)' } } )
    
    x = dbaze.runQuery( 'query 1' )
    print x
    
    if dbaze.showForm( 'frmA' )[ 'success' ]:
        print "Form frmA has been shown!"
    
    if dbaze.showReport( 'rprtZ' )[ 'success' ]:
        print "Report rprtZ has been shown!"
    
    x = dbaze.runScript( 'my r script' )
    print x
    
    # bonus:
    
    q = dbaze.queries[ 'query 2' ]
    print q.type
    print q.query
    q.run()
    
    f = dbaze.forms[ 'frmA' ]
    f.show()
    
    r = dbaze.reports[ 'rprtZ' ]
    r.show()
    
    s = dbaze.scripts[ 'my r script' ]
    print s.type
    print s.script
    s.run()
