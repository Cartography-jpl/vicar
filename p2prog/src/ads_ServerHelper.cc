/*
 * ads_ServerHelper.cc
 */

#include "ads_ServerHelper.h"

#include "jpl_mipl_spice_corba.C.h"
    
#include "ads_ORB.h"


typedef jpl::mipl::spice::corba::SpiceLibFactory SpiceLibFactory;
typedef jpl::mipl::spice::corba::SpiceLibFactory_var SpiceLibFactory_var;
typedef PortableServer::POAManager_var POAManager_var;
typedef PortableServer::Servant Servant;
typedef PortableServer::POA POA;



ads_ServerHelper::ads_ServerHelper(Servant servant) 
  : i_own_the_orb_(0),
    has_been_deactivated_(0)
{
   init(servant);
}

ads_ServerHelper::ads_ServerHelper(int& argc, 
                                   char* argv[], 
                                   Servant servant)
   : i_own_the_orb_(1)
{
   
   ads_ORB::init(argc, argv);
   init(servant);
}

ads_ServerHelper::~ads_ServerHelper()
{
  //  We don't call fini() here since the ORB may have already been
  //  shut down before this object is destroyed.
  //
  // fini();
}    




 
void ads_ServerHelper::fini()
{
  if (!has_been_deactivated_)
  {
    deactivate();
  }

  if (i_own_the_orb_)
  {
    i_own_the_orb_ = 0;
    ads_ORB::instance()->destroy();
  }
}

void ads_ServerHelper::run()
{
  ads_ORB::instance()->run();
}

void ads_ServerHelper::halt()
{
  if (!has_been_deactivated_)
  {
    deactivate();
  }
  ads_ORB::instance()->shutdown(0);
}

void ads_ServerHelper::deactivate()
{
  has_been_deactivated_ = 1;
  
   ACE_DECLARE_NEW_CORBA_ENV;
   ACE_TRY
   {
      PortableServer::ObjectId_var id = poa_->servant_to_id(servant_,
							    ACE_TRY_ENV);
      ACE_TRY_CHECK;
      poa_->deactivate_object(id.in(), ACE_TRY_ENV);
      ACE_TRY_CHECK;
      POAManager_var thePoaManager = poa_->the_POAManager(ACE_TRY_ENV);
      ACE_TRY_CHECK;
      thePoaManager->deactivate(0, 0, ACE_TRY_ENV);
      ACE_TRY_CHECK;
      
   }
   ACE_CATCH (PortableServer::POA::WrongPolicy, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: POA::WrongPolicy\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("WrongPolicy");
   }
   ACE_CATCH (PortableServer::POA::ServantNotActive, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: POA::ServantNotActive\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("ServantNotActive");
   }        
   ACE_CATCH (PortableServer::POA::ObjectNotActive, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: POA::ObjectNotActive\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("ObjectNotActive");
   }         
   ACE_CATCH (PortableServer::POAManager::AdapterInactive, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: POAManager::AdapterInactive\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("AdapterInactive");
   }          
   ACE_ENDTRY;
}
    
void ads_ServerHelper::init(PortableServer::Servant servant)
{            
   servant_ = servant;
   
   ACE_DECLARE_NEW_CORBA_ENV;
   ACE_TRY
   {
      poa_ = POA::_narrow(
	 ads_ORB::instance()->resolve_initial_references("RootPOA",
							 ACE_TRY_ENV),
	 ACE_TRY_ENV);
      ACE_TRY_CHECK;
      PortableServer::POAManager_var poaManager = poa_->the_POAManager(
	 ACE_TRY_ENV);
      ACE_TRY_CHECK;
      poa_->activate_object(servant_, ACE_TRY_ENV);
      ACE_TRY_CHECK;
      poaManager->activate(ACE_TRY_ENV);
      ACE_TRY_CHECK;
            
      servantObject_ = poa_->servant_to_reference(servant_, ACE_TRY_ENV);
      ACE_TRY_CHECK;
   }
   ACE_CATCH (CORBA::ORB::InvalidName, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: ORB::InvalidName\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("InvalidName");
   }
   ACE_CATCH (PortableServer::POA::ServantAlreadyActive, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: POA::ServantAlreadyActive\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("ServantAlreadyActive");
   }
   ACE_CATCH (PortableServer::POA::WrongPolicy, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: POA::WrongPolicy\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("WrongPolicy");
   }
   ACE_CATCH (PortableServer::POAManager::AdapterInactive, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: POAManager::AdapterInactive\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("AdapterInactive");
   }
   ACE_CATCH (PortableServer::POA::ServantNotActive, e)
   {
      ACE_ERROR (( LM_ERROR, "Error: POA::ServantNotActive\n" ));
      ACE_OS::exit(1);
      // throw new RuntimeException("ServantNotActive");
   }        
   ACE_ENDTRY;
}


void ads_ServerHelper::writeToFile(const ACE_CString& fileName)
{
   CORBA::String_var str = 
      ads_ORB::instance()->object_to_string(servantObject_.in());

	 FILE* ior_output_file = ACE_OS::fopen (fileName.c_str(), "w");
	 if (ior_output_file == 0)
	 {
	    ACE_ERROR ((LM_ERROR,
                   "%s%s%s\n",
                   "Error writing IOR to file '", fileName.c_str(), "'."));
	    ACE_OS::exit(1);
	 }
	 
	 ACE_OS::fprintf (ior_output_file,
                     "%s\n",
                     str.in ());
	 ACE_OS::fclose (ior_output_file);
}


void ads_ServerHelper::writeToFile(int& argc, char* argv[])
{
   const ACE_CString FILE_FLAG = "-o";
   
   // Check to make sure -o isn't the last option on the command line.
   
   if ((argc > 0) && (ACE_CString(argv[argc - 1]) == FILE_FLAG))
   {
      ACE_ERROR (( LM_ERROR, 
		   "Error:  "
		   "The '-o' option must be followed by a file name.\n" ));
      ACE_OS::exit(1);

      // throw new IllegalArgumentException(
      //  "The '-o' option must be followed by a file name.");
   }
   
   for (int i=0; i < argc - 1; ++i)
   {
      if (ACE_CString(argv[i]) == FILE_FLAG)
      {
         writeToFile(argv[i + 1]);
      }
   }
}   

PortableServer::POA_var ads_ServerHelper::thePoa()
{
  return poa_;
}
