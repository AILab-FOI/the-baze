Intro
-----

The Baze is a database development environment similar to MS Access, 
LibreOffice Base, Query Anaylzer and similar. It aims on being a Swiss
army knife for the data scientist.

It supports a number of SQL and NoSQL databases (PostgreSQL, SQLite, 
ODBC; MongoDB, eXist DB, XSB, Flora-2, DES, ZODB) and support for other 
systems is pending.

It supports a number of query languages (SQL, Datalog, Py-MapReduce,
xPath, xQuery, transactional frame logic, Mongo JavaScript) and implements 
an experimental scripting interface in Python, NodeJS and the R language. 

Creation of user designed forms is partially supported (experimental)
and support for report creation is pending.

It is implemented in Gambas3, not fully functional yet, but a lot
of functionality is already implemented. It uses the Datalog Educational
System (DES - see http://des.sourceforge.net) to supply Datalog queries 
for all types of relational databases that are supported.

A recent binary can be downloaded from:

http://tinyurl.com/skini-baze

but only the source has the latest features.

How to compile
--------------

In order to compile from source you need the latest Gambas3 packages
from the official ppa (as from October 2016). To add the ppa to Ubuntu
type:

```
$ sudo add-apt-repository -y ppa:gambas-team/gambas-daily
$ sudo apt-get update
```

Also to use the NodeJS scripting interface, you need the newest version
of NodeJS. To add the repository use:

```
$ curl -sL https://deb.nodesource.com/setup_6.x -o nodesource_setup.sh
$ sudo bash nodesource_setup.sh
$ rm nodesource_setup.sh
```

Then install the following packages:

```
$ sudo apt-get -y install gambas3 gambas3-gb-qt5-webkit gambas3-gb-qt5 git python-zodb python-jsonpickle swi-prolog swi-prolog-odbc unixodbc odbcinst r-base r-cran-rjson nodejs gambas3-gb-qt4-webkit mongodb-org-shell
```

Now you can compile the-baze with:

```
$ cd the-baze
$ make
```

If everything is OK, you can install it to /usr/bin with:

```
$ sudo make install
```

You should now be able to run the-baze with:

```
$ baze
```

