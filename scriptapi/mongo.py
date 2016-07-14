#!/usr/bin/env python

#-*- coding: utf-8 -*-

from pymongo import MongoClient
from pymongo.database import Database, Collection
from bson.objectid import ObjectId

from json import loads, dumps

class MongoTheBaze( MongoClient ):
    def __init__( *args, **kwargs ):
        MongoClient.__init__( *args, **kwargs )
        self = args[ 0 ]
        def myget( *args, **kwargs ):
            item = args[ 0 ].__getattr__( *args, **kwargs )
            print 'myget item:', item
            item.find = lambda *a, **kw: dumps( [ i for i in item.find( *a, **kw ) ] )
            return item
        def mygetdb( *args, **kwargs ):
            item = args[ 0 ].__getattr__( *args, **kwargs )
            print 'mygetdb item:', item
            for c in item.collection_names():
                item[ c ].__getattr__ = myget
            return item
        for db in self.database_names():
            self[ db ].__getattr__ = mygetdb
            print self[ db ][ 'foo' ]
            for c in self[ db ].collection_names():
                print self[ db ][ c ].find
                print self[ db ][ c ].find( {} )
    
    def show_dbs( self ):
        return self.database_names()
        
    def show_dbs_json( self ):
        return dumps( self.show_dbs() )
    
    def show_cols( self, db ):
        db = Database( self, db )
        return db.collection_names()
    
    def show_cols_json( self, db ):
        return dumps( self.show_cols( db ) )
    
    def query( self, db, col, query ):
        lst = [ i for i in self[ db ][ col ].find( query ) ]
        for i in lst:
            for k, v in i.items():
                if type( v ) == ObjectId:
                    i[ k ] = str( v )
        return lst
    
    def query_json( self, db, col, query ):
        return dumps( self.query( db, col, query ) )
    
    def find( *args, **kwargs ):
        r = MongoClient


if __name__ == '__main__':
    c = MongoTheBaze( 'localhost', 27017 )
    for db in c.show_dbs():
        print db, ':',
        print c.show_cols_json( db )
    
    db = 'foo'
    col = 'bar'
    
    print c.query( db, col, {} )
    print c.query_json( db, col, {} )
    
    print [ i for i in c.foo.bar.find( {} ) ]