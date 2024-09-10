// ads_error.cc
//
// Apr 18, 2002
// Michael Brady

#include "ads_error.h"

#include "jpl_mipl_spice_corba.C.h"

#include "SpiceUsr.h"
#include "SpiceZfc.h" // for trcdep_ and trcnam_


// Strings for calling getmsg_c

static const char* SHORT_STR = "SHORT";
static const char* EXPLAIN_STR = "EXPLAIN";
static const char* LONG_STR = "LONG";

// The lengths below are specified in getmsg_c.c

static const int MAX_SHORT_MESSAGE_LENGTH = 25;
static const int MAX_DESCRIPTION_LENGTH   = 80;
static const int MAX_LONG_MESSAGE_LENGTH  = 1840;

void ads_checkForError ()
{
    if (failed_c())
    { 
       // Find the error info.
       
       char name[MAX_SHORT_MESSAGE_LENGTH + 1];
       char description[MAX_DESCRIPTION_LENGTH + 1];
       char longMessage[MAX_LONG_MESSAGE_LENGTH + 1];
       
       memset(name, '\0', sizeof(name));
       memset(description, '\0', sizeof(description));
       memset(longMessage, '\0', sizeof(longMessage));
       
       
       getmsg_c(SHORT_STR, sizeof(name), name);
       getmsg_c(EXPLAIN_STR, sizeof(description), description);
       getmsg_c(LONG_STR, sizeof(longMessage), longMessage);


       jpl::mipl::spice::corba::SpiceLib::ToolkitException ex;
       ex.name = CORBA::string_dup(name);
       ex.description = CORBA::string_dup(description);
       ex.longMessage = CORBA::string_dup(longMessage);

       
       SpiceInt traceDepth = 0;
       trcdep_(&traceDepth);

       ex.trace.length(traceDepth);
      
       for (SpiceInt i=1; i <= traceDepth; ++i)
       {
          char functionName[128];
          memset(functionName, '\0', sizeof(functionName));
          trcnam_(&i, functionName, sizeof(functionName));

          // Chop off the string:
          //  (1) at the first newline.
          //  (2) at the first space.
          //  (3) if all else fails, at the end of the buffer.
          for (unsigned int c=0; c < sizeof(functionName); ++c)
          {
             if (functionName[c] == ' ')
             {
                functionName[c] = '\0';
                break;
             }
             else if (functionName[c] == '\n')
             {
                functionName[c] = '\0';
                break;
             }
             else if (c == (sizeof(functionName) - 1))
             {
                functionName[c] = '\0';
                break; 
             }
          }

          ex.trace[static_cast<size_t>(i-1)] = CORBA::string_dup(functionName);
       }

      // Reset the SPICE error mechanism so subsequent 
      // SPICE calls can proceed.

      reset_c();


       throw ex;
    }
}
