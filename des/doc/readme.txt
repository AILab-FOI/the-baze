//------------------------------//
//          ACIDE 0.17          //
//        September, 10th 2015  //
//------------------------------//

System Requirements:

  - Installation of Java JRE 1.6 or later.
  - Installation of PDF reader software to display help files.


Running ACIDE:

  - Windows. Alternatives:
    * Double click on acide.jar.
    * Type from an OS command shell one of the following:
        java -jar des_acide.jar 
        javaw -jar des_acide.jar (no shell)
        
  - Other OS's:
    Type from an OS command shell:
      java -jar des_acide.jar 


For developers:

The source code has been completely cleaned and refactored from the 0.16 version source code using Eclipse IDE version 4.2.

The project file for Eclipse 4.2 is also available with the current distribution of ACIDE - A Configurable IDE.
To start editing its source code in order to make your own distribution of ACIDE - A Configurable IDE you simply
have to import the project into your Eclipse IDE. 

For developers who are going to use Eclipse for the development of ACIDE:
The class com.thoughtworks.xstream.converters.reflection.Sun14ReflectionProvider is going to 
throw an error like The type [X] is not accessible due to restriction on required library [Y].

In order to fix it you will have to configure the compilance settings of Eclipse to set that
kind of exceptions like warnings and not as errors:
	--> Deprecated and restricted API		
	--> Forbidden reference --> Warning
