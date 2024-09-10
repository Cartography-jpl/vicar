// ads_Body.cc
//
// April 9, 2002
// Michael Brady

#include "ads_Body.h"

#include "ads_error.h"
#include "ads_TraceAdder.h"

const ads_Body& ads_Body::sun()
{
   ADS_TRACE;

  static const SpiceInt SUN_NAIF_ID = 10;
  static ads_Body sun(SUN_NAIF_ID);
  return sun;
}



ads_Body::ads_Body(SpiceInt naifId)
  : m_naifId(naifId)
{
   ADS_TRACE;
}
  
SpiceInt ads_Body::naifId() const
{
  return m_naifId;
}

bool ads_Body::isMoon() const
{
  return 
    
    // The body must be between 100 and 1000.
    
    ((m_naifId > 100) && (m_naifId < 1000))
    &&

    // It must not end in '00' (for example 200, 300, or 400).

    ((m_naifId % 100) != 0)
    &&


    // It must not end in '99' (for example 299, 399, or 499),
    // which is reverved for the planet itself.

    ((m_naifId % 100) != 99);
}


ads_Body ads_Body::centerPlanet() const
{
  if (!isMoon())
  {
    return ads_Body( 0 );
  }
  
  return ads_Body( m_naifId - (m_naifId % 100) + 99 );
}

ads_Body ads_Body::spacecraft() const
{
  return ads_Body( m_naifId / 1000 );
}

ads_Frame ads_Body::scFrame() const
{
   return ads_Frame( m_naifId * 1000);
}


string ads_Body::frameName() const
{
   ADS_TRACE;

  SpiceChar name[128];
  memset(name, '\0', sizeof(name));
  SpiceInt code = 0;
  SpiceBoolean wasFound = 0;

  cidfrm_c ( m_naifId,
             sizeof(name),
             &code,
             name,
             &wasFound );
  ads_checkForError();

  return string(name);
}

string ads_Body::name() const
{
   ADS_TRACE;

   SpiceChar buf[128];
   memset(buf, '\0', sizeof(buf));
   SpiceBoolean wasFound = 0;

   bodc2n_c (m_naifId,
             sizeof(buf),
             buf,
             &wasFound);
   ads_checkForError();

   return string(buf);
}
