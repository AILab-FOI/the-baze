var fs = require('fs');

function query( parent, name )
{
    this.parent = parent
    this.name = name
}

form = report = script = query

function sleep( seconds )
{
    setTimeout( function(){}, seconds * 1000 );
}

function baze( infile, outfile, queries, forms, reports, scripts )
{
    this.infile = infile;
    
    this.outfile = outfile;
    
    this.queries = new Array;
    
    for( var q in queries )
    {
        var qu = new query( this, queries[ q ].name );
        qu.run = function()
        {
            return this.parent.runQuery( this.name );
        }
        qu.type = queries[ q ].type
        qu.query = queries[ q ].query
        this.queries[ queries[ q ].name ] = qu
    }
    
    this.forms = new Array;
    
    for( var f in forms )
    {
        var fo = new form( this, forms[ f ].name );
        fo.show = function()
        {
            return this.parent.showForm( this.name );
        }
        this.forms[ forms[ f ].name ] = fo
    }
    
    this.reports = new Array;
    
    for( var r in reports )
    {
        var re = new report( this, reports[ r ].name );
        re.show = function()
        {
            return this.parent.showReport( this.name );
        }
        this.reports[ reports[ r ].name ] = re
    }
    
    this.scripts = new Array;
    
    for( var s in scripts )
    {
        var sc = new script( this, scripts[ s ].name );
        sc.run = function()
        {
            return this.parent.runScript( this.name );
        }
        sc.type = scripts[ s ].type
        sc.script = scripts[ s ].script
        this.scripts[ scripts[ s ].name ] = sc
    }    
}

baze.prototype.getResult = function( rid )
{
    while( true )
    {
        var js = JSON.parse( fs.readFileSync( this.infile, 'utf8' ) );
        if ( hasOwnProperty.call( js, rid ) )
        {
            var result = js[ rid ];
            break;
        }
        else
        {
            sleep( 0.1 );
        }
    }
    delete js[ rid ];
    fs.writeFileSync( this.infile, JSON.stringify( js ), 'utf8', function( err )
    {
        console.log( err );
    } );
    return result;
}

baze.prototype.sendRequest = function( typ, oid, command, params )
{
    var req = {
        type: typ,
        oid: oid
    };
    if( command != null )
    {
        req.command = command;
    }
    if( params != null )
    {
        req.params = params;
    }
    fs.writeFileSync( this.outfile, JSON.stringify( req ), 'utf8', function( err )
    {
        console.log( err );
    } );
}

baze.prototype.runQuery = function( query )
{
    this.sendRequest( 'runquery', query );
    var res = this.getResult( query );
    // TODO: there seems to be a bug here (or maybe somwhere else), when a query is run (usually the first time) an "Unexpected end of JSON input" error is thrown. I'm guessing this has something to do with the asynchronous nature of nodejs, but I might be wrong. Need to inspect this!
    return res;
} 

baze.prototype.showForm = function( form )
{
    this.sendRequest( 'showform', form );
    return this.getResult( form );
} 

baze.prototype.showReport = function( report )
{
    this.sendRequest( 'showreport', report );
    return this.getResult( report );
} 

baze.prototype.runScript = function( script )
{
    this.sendRequest( 'runscript', script );
    return this.getResult( script );
} 

exports.baze = baze

/*
dbaze = new baze( 
        'scriptin.json',
        'scriptout.json', 
        { 
            'query 1':{ 'name':'query 1', 'query':'select * from a', 'type':'SQL' }, 
            'query 2':{ 'name':'query 2', 'query':'select * from a', 'type':'SQL' }
        }, 
        { 'frmA':{ 'name':'frmA' }, 'frmB':{ 'name':'frmB' } }, 
        { 'rprtX':{ 'name':'rprtX' }, 'rprtY':{ 'name':'rprtY' }, 'rprtZ':{ 'name':'rprtZ' } }, 
        { 'my r script':{ 'name':'my r script', 'type':'R', 'script':'print(1)' } } );

dbaze.runQuery( 'query 1' )

if( dbaze.showForm( 'frmA' ).success )
    console.log( 'Form frmA has been shown!' )

if( dbaze.showReport( 'rprtY' ).success )
    console.log( 'Report rprtY has been shown!' )

dbaze.runScript( 'my r script' )

var q = dbaze.queries[ 'query 1' ]
q.run()
console.log( q )

var f = dbaze.forms[ 'frmA' ]
f.show()

var r = dbaze.reports[ 'rprtZ' ]
r.show()

var s = dbaze.scripts[ 'my r script' ]
s.run()
console.log( s )*/
