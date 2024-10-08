************************************************************************
* vicset1_projects.h
*
* Given the Project (defined via a PROJ_xxx symbol), set up the Products
* (PROD_xxx), Subsystems (SS_xxx) and externals (EXT_xxx and JEXT_xxx)
* that are needed by the project.
*
* Note that only direct dependencies should be listed here... let the
* dependencies in vicset1_depends.h pick up the indirect dependencies.
* For example, if the Project needs MATIS, it should define PROD_MATIS...
* not JEXT_JBPM directly.  Thats because the Project doesnt really care
* whether JBPM is there or not... it just wants MATIS.  Although MATIS
* itself needs JBPM, that is set up in vicset1_depends.h.
*
* However, that is not always practical, and it is acceptable to define
* subsystems and externals here if needed.  For example, there is not
* yet enough granularity in P2 to be useful, and we dont want to include
* all externals anything in P2 uses.  So, define those externals needed by
* the pieces of P2 that are needed by the Project.
*
************************************************************************
*
* For the "ALL" project, we turn everything on.  While turning on all
* products is overkill if we also turn on all the subsystems and externals,
* this provides a handy reference for all available products, subsystems,
* and externals.
*
************************************************************************

#ifdef PROJ_ALL
*--------
* Non-Java products
*--------
#define PROD_MARS_SURFACE
#define PROD_PIG
*
#define PROD_XVD
*
#define PROD_FEI3
#define PROD_FEI5
#define PROD_DBQ
#define PROD_DBVIEW
#define PROD_PWDSERVER
#define PROD_PWDSERVER_CLIENT
#define PROD_DBI
#
#define PROD_TLM_PEWRAPPER
*
#define PROD_VICAR_EXEC
*
*--------
* Java products
*--------
#define PROD_CDRS
#define PROD_QViewer
*
#define PROD_JavaVicarIO
#define PROD_JadeDisplay
#define PROD_JADIS
#define PROD_Marsviewer
#define PROD_SITH
#define PROD_POSSUM
#define PROD_MICA
*
#define PROD_MER_RMC
*
*  theres also PROD_FEI5 covering both client and server...
#define PROD_FEI5_Client
#define PROD_FEI5_Server
#define PROD_FEI5_Savannah
#define PROD_FEI5_Spider
#define PROD_PWDSERVER_JCLIENT
#define PROD_FEI5_Client_MSL
*
#define PROD_MATIS_CORE
#define PROD_MATIS_PHX
#define PROD_MATIS_MSL
#define PROD_MATIS_Diviner
*
#define PROD_SPICE_CORBA
#define PROD_SPICE_JNI
*
#define PROD_JEDI
#define PROD_TLM_JavaCore
#define PROD_TLM_MRO
#define PROD_TLM_SIRTF
*
#define PROD_UPLINK_CASSINI
#define PROD_UPLINK_CAS_TOUR
*
#define PROD_Java_XPM
*
#define PROD_PLACES
*
*--------
* VICAR Subsystems
*--------
#define SS_TAE
#define SS_RTL
#define SS_MOTIFAPP
#define SS_CAS
#define SS_CASCRUISE
#define SS_FEI5
#define SS_GUI
#define SS_SHVIC
#define SS_STAE
#define SS_P1PROG
#define SS_P1SUB
#define SS_P2PROG
#define SS_P2SUB
#define SS_DIV
#define SS_SIRTF
#define SS_TLM
#define SS_UPLINK
#define SS_UPLINKTOUR
#define SS_SSV
#define SS_NEAT
#define SS_P3PROG
#define SS_P3SUB
#define SS_V2DATA
#define SS_V2CONFIG
#define SS_MARS
#define SS_VRDI
#define SS_VIDS
#define SS_MDMS
#define SS_FEI
#define SS_JAVA
#define SS_PWDSERVER
*
*--------
* Non-Java Externals
*--------
#define EXT_SPICE
#define EXT_SYBASE
#define EXT_JNI
#define EXT_CALLABLE_JDK
#define EXT_GEOTRANS
#define EXT_PDS
#define EXT_CASISS
#define EXT_ISIS
#define EXT_PDS_LABEL
#define EXT_TIFF
#define EXT_SIMBAD
#define EXT_MATRACOMP
#define EXT_PVM
#define EXT_PGPLOT
#define EXT_KERBEROS5
#define EXT_KERBEROS4
#define EXT_ACE_TAO
#define EXT_XERCES_CXX
#define EXT_ROGUEWAVE
#define EXT_CANDELA
#define EXT_MATH77
#define EXT_MPI
#define EXT_HDF
#define EXT_MERIDD
#define EXT_SIRTF_PHE
#define EXT_EXPAT
#define EXT_SUMMITT
#define EXT_MYSQL
*--------
* Java Externals
*--------
#define JEXT_ORBACUS
#define JEXT_COSNOTIFY
#define JEXT_XERCES
#define JEXT_XALAN
#define JEXT_J2EE
#define JEXT_OODT
#define JEXT_JCONNECT
#define JEXT_JAKARTA_ORO
#define JEXT_UTIL_CONCURRENT
#define JEXT_JDOM
#define JEXT_AMMOSDOM
#define JEXT_JWSDP
#define JEXT_JAXWS
#define JEXT_JAXB
#define JEXT_LOG4J
#define JEXT_FSCONTEXT
#define JEXT_JAXEN
#define JEXT_COMMON_LOGGING
#define JEXT_COMMON_COLLECTIONS
#define JEXT_COMMON_POOL
#define JEXT_COMMON_DBCP
#define JEXT_JGROUPS
#define JEXT_HIBERNATE2
#define JEXT_HIBERNATE
#define JEXT_XDOCLET
#define JEXT_ANT
#define JEXT_JAVAMAIL
#define JEXT_JAF
#define JEXT_NOM_TAM_FITS
#define JEXT_C3P0
#define JEXT_DOM4J
#define JEXT_JBOSS
#define JEXT_JOGL
#define JEXT_JBPM
#define JEXT_MYSQL
#define JEXT_ATLAS_SERVICE
#define JEXT_JSCH
#define JEXT_ACTIVEMQ
#define JEXT_DERBY
*
#endif
*
************************************************************************
*
* CASSINI
*
************************************************************************
#ifdef PROJ_CAS
*
#define PROD_XVD
*
#define PROD_FEI3
#define PROD_FEI5
#define PROD_FEI5_Savannah
#define PROD_PWDSERVER
#define PROD_PWDSERVER_CLIENT
#define PROD_PWDSERVER_JCLIENT
#define PROD_DBQ
#define PROD_DBVIEW
*
#define PROD_SPICE_CORBA
#define PROD_SPICE_JNI
*
#define PROD_CDRS
#define PROD_QViewer
#define PROD_JavaVicarIO
#define PROD_JadeDisplay
#define PROD_JADIS
*
#define PROD_VICAR_EXEC
#define SS_TAE
*
#define SS_CAS
#define SS_CASCRUISE
#define SS_V2CONFIG
#define SS_V2DATA
#define SS_P1PROG
#define SS_P2PROG
#define SS_P3PROG
#define SS_TLM
#define SS_UPLINK
#define SS_UPLINKTOUR
#define SS_VIDS
#define SS_VRDI
*
#define EXT_SPICE
#define EXT_TIFF
#define EXT_MATRACOMP
#define EXT_CASISS
#define EXT_ISIS
#define EXT_PGPLOT
#define EXT_PVM
#define EXT_KERBEROS5
#define EXT_PDS_LABEL
#define EXT_MATH77
*
#endif
*
************************************************************************
*
* DIVINER
*
************************************************************************
#ifdef PROJ_DIVINER
*
#define PROD_XVD
*
#define PROD_FEI5
#define PROD_FEI5_Savannah
*
#define PROD_FEI3
#define PROD_DBQ
*
#define PROD_JEDI
*
#define SS_P2PROG
#define SS_P1PROG
#define PROD_VICAR_EXEC
#define SS_TAE
*
#define SS_DIV
*
#define PROD_JavaVicarIO
#define PROD_JadeDisplay
*
#define PROD_MATIS_Diviner
#
#define EXT_SPICE
#define EXT_PDS_LABEL
#define EXT_MATH77
#endif
*
************************************************************************
*
* MARS SURFACE MISSIONS
*
* MER, PHX, MSL are almost identical, so theyre defined together here.
* Any differences are listed separately at the bottom.
*
************************************************************************
#if defined(PROJ_MER) || defined(PROJ_PHX) || defined(PROJ_MSL)
*
#define PROD_XVD
*
#define PROD_MARS_SURFACE
#define SS_P1PROG
#define SS_P2PROG
#define SS_P3PROG
*
#define SS_TAE
#define SS_SSV
#define SS_VIDS
*
#define PROD_FEI5
#define PROD_FEI5_Savannah
#define PROD_JEDI
*
#define PROD_JADIS
#define PROD_Marsviewer
#define PROD_SITH
#define PROD_MICA
*
#define EXT_MATH77
#define EXT_PVM
#define EXT_SPICE
#define EXT_TIFF
#define EXT_PDS_LABEL
#define EXT_PGPLOT	/* only for rovernav, eventually obsolete */
#define EXT_EXPAT	/* only for xmln, whatever that is... */
*
#endif
*
*** MER only
*
#ifdef PROJ_MER
#define PROD_PLACES
#define PROD_FEI3
#define PROD_DBQ
#define PROD_MER_RMC
#endif
*
*** PHX only
*
#ifdef PROJ_PHX
#define PROD_MATIS_PHX
#define SS_V2CONFIG
#define PROD_FEI3
#define PROD_DBQ
#endif
*
*** MSL only
*
#ifdef PROJ_MSL
#define PROD_PLACES
#define PROD_MATIS_MSL
#define SS_V2CONFIG
#define PROD_FEI3
#define PROD_DBQ
#define PROD_FEI5_Client_MSL
#endif
*
************************************************************************
*
* MRO
*
************************************************************************
#ifdef PROJ_MRO
*
#define PROD_FEI5
#define PROD_FEI5_Savannah
#define PROD_FEI5_Spider
*
#define PROD_TLM_MRO
*
#define PROD_DBVIEW
*
#endif
*
************************************************************************
*
* SIRTF/Spitzer
*
************************************************************************
#ifdef PROJ_SIRTF
*
#define SS_SIRTF
*
#define PROD_FEI3
*
#define PROD_DBVIEW
#define PROD_PWDSERVER
#define PROD_PWDSERVER_CLIENT
#define PROD_TLM_SIRTF
#define PROD_JavaVicarIO
#define PROD_VICAR_EXEC
*
#define SS_TAE
*
#endif
*
************************************************************************
*
* MGSS INTEGRATED SYSTEM  (no Projects)
*
************************************************************************
#ifdef PROJ_MGSS
*
*--------
* Non-Java products
*--------
#define PROD_XVD
*
#define PROD_FEI3
#define PROD_FEI5
#define PROD_DBQ
#define PROD_DBVIEW
#define PROD_PWDSERVER
#define PROD_PWDSERVER_CLIENT
#define PROD_DBI
#
#define PROD_VICAR_EXEC
*
*--------
* Java products
*--------
#define PROD_JavaVicarIO
#define PROD_JadeDisplay
#define PROD_JADIS
#define PROD_Marsviewer
#define PROD_SITH
*
*  theres also PROD_FEI5 covering both client and server...
#define PROD_FEI5_Client
#define PROD_FEI5_Server
#define PROD_FEI5_Savannah
#define PROD_FEI5_Spider
#define PROD_PWDSERVER_JCLIENT
*
#define PROD_MATIS_CORE
#define PROD_MATIS_MSL
*
#define PROD_SPICE_CORBA
#define PROD_SPICE_JNI
*
#define PROD_JEDI
#define PROD_TLM_JavaCore
*
#define PROD_Java_XPM
*
*--------
* VICAR Subsystems
*--------
#define SS_TAE
#define SS_P1PROG
#define SS_P1SUB
#define SS_P2PROG
#define SS_P2SUB
#define SS_P3PROG
#define SS_P3SUB
#define SS_TLM
#define SS_SSV
#define SS_V2DATA
#define SS_V2CONFIG
#define SS_VRDI
#define SS_VIDS
#define SS_MDMS
#define SS_FEI
#define SS_JAVA
#define SS_PWDSERVER
*
*--------
* Non-Java Externals
*--------
#define EXT_SPICE
#define EXT_SYBASE
#define EXT_JNI
#define EXT_CALLABLE_JDK
#define EXT_GEOTRANS
#define EXT_PDS
#define EXT_ISIS
#define EXT_PDS_LABEL
#define EXT_TIFF
#define EXT_SIMBAD
#define EXT_PVM
#define EXT_PGPLOT
#define EXT_KERBEROS5
#define EXT_KERBEROS4
#define EXT_ACE_TAO
#define EXT_XERCES_CXX
#define EXT_ROGUEWAVE
#define EXT_CANDELA
#define EXT_MATH77
#define EXT_MPI
#define EXT_HDF
#define EXT_EXPAT
#define EXT_SUMMITT
#define EXT_MYSQL
*--------
* Java Externals
*--------
* ... taken care of by dependencies...
*
#endif
*
************************************************************************

