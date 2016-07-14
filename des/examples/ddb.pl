:-type(p(a:int)).
:-type(q(b:int)).
:-type(r(a:int,b:int,c:string)).
:-pk(p,[a]).
:-pk(q,[b]).
:-fk(r,[a],p,[a]).
:-fk(r,[b],q,[b]).
