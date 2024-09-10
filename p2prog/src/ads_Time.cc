// ads_Time.cc
//
// April 9, 2002
// Michael Brady

#include "ads_Time.h"
#include "ads_error.h"
#include "ads_TraceAdder.h"

ads_Time::ads_Time(const char* utcTime)
{
   ADS_TRACE;
   
   utc2et_c(utcTime, &m_ephemeris);
   ads_checkForError();
}

SpiceDouble ads_Time::spacecraftClock(SpiceInt spacecraftId)
{
   ADS_TRACE;

   SpiceDouble ret = 0.0;
   sce2c_c ( spacecraftId, m_ephemeris, &ret );
   ads_checkForError();
   return ret;
}

SpiceDouble ads_Time::ephemeris()
{
   ADS_TRACE;
  
   return m_ephemeris;
}
