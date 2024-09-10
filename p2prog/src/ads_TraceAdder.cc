// ads_TraceAdder.cc
//
// Apr 16, 2002
// Michael Brady

#include "ads_TraceAdder.h"

#include "SpiceUsr.h"

ads_TraceAdder::ads_TraceAdder(const char* fileName, const char* functionName)
   : m_functionName(fileName)
{
   // Since the function name length is limited, cut down the file name.

   if (m_functionName.length() > 7)
   {
      // Strip off the leading 'ads_' and  the trailing '.cc'.

      m_functionName = m_functionName.substr(4, m_functionName.length() - 7);
   }

   m_functionName += ":";
   m_functionName += functionName;

   chkin_c(m_functionName.c_str());
}


ads_TraceAdder::~ads_TraceAdder()
{
   chkout_c(m_functionName.c_str());
}
