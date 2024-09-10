// ads_server.cc
//
//  The main routine for the server.

#include "ads_SpiceLib_i.h"
#include "ads_ServerHelper.h"


int main(int argc, char* argv[])
{
  try
  {
    if (argc > 1 && (strcmp(argv[1], "-h") == 0))
    {
      ACE_ERROR ((
                  LM_ERROR,
                  "usage:  %s"
                  " [-o <ior_output_file>]"
                  "\n",
                  argv [0]));
      ACE_OS::exit(1);
    }
    
    
    ads_SpiceLib_i spiceLib;
    ads_ServerHelper helper(argc, argv, &spiceLib);
    
    helper.writeToFile(argc, argv);

    cout << "Running..." << endl;
    helper.run();
    cout << "Finished run." << endl;
  }
  catch (CORBA::UserException& ex)
  {
    ACE_PRINT_EXCEPTION (ex, 
                         "\nError: UserException:\n");
    return 1;    
  }
  catch (CORBA::SystemException& ex)
  {
    ACE_PRINT_EXCEPTION (ex, 
                         "\nError:  Network error:\n");
    return 2;
  }

  return 0;
}
