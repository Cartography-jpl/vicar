// ads_DefaultGeometryFinder.cc
//
//  April 9, 2002
//  Michael Brady

#include "ads_DefaultGeometryFinder.h"

#include "ads_Time.h"
#include "ads_Body.h"
#include "ads_error.h"
#include "jpl_mipl_spice_corba.C.h"
#include "ads_TraceAdder.h"

/////////////////////////////////////////////////////////////////////
// Local statics
/////////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////////////
// Class implementation
/////////////////////////////////////////////////////////////////////


ads_DefaultGeometryFinder::~ads_DefaultGeometryFinder()
{
}

void ads_DefaultGeometryFinder::getGeometry ( int instNaifId,
                                          int targetNaifId,
                                          const char* utcTime,
                                          const char* referenceFrame,
                                          double tolerance,
                                          Geometry_out theGeometry,
                                          GeometryMetaData_out metaData )
  throw ( SpiceLib::ToolkitException,
          MiplSpiceLib::PointingNotFound )
{
   ADS_TRACE;


  const SpiceDouble DEFAULT_TOLERANCE = 80.0;

  ads_Time epoch(utcTime);
  ads_Body instrument(instNaifId);
  ads_Body target(targetNaifId);

  // Find the instrument pointing and velocity.

  tolerance = 
    ((tolerance >= 0.0) ? tolerance : DEFAULT_TOLERANCE);
  SpiceDouble timeFound;
  SpiceBoolean wasFound;

  ckgpav_c(instrument.naifId(),
           epoch.spacecraftClock(instrument.spacecraft().naifId()),
           tolerance,
           referenceFrame,
           theGeometry.instrumentPointing,
           theGeometry.spacecraftVelocityVector,
           &timeFound,
           &wasFound);
  ads_checkForError();

  if (!wasFound)
  {
    throw MiplSpiceLib::PointingNotFound();
  }

  metaData->facility = "NAIF";


  // Find the spacecraft vector.

  SpiceDouble lightTime = 0.0;
  memset(theGeometry.spacecraftVector, 0, 
         sizeof(theGeometry.spacecraftVector));

  spkez_c ( target.naifId(),
            epoch.ephemeris(),
            referenceFrame,
            "LT+S",
            instrument.spacecraft().naifId(),
            theGeometry.spacecraftVector,
            &lightTime );
  ads_checkForError();
    
	      

  // If the target is a moon, find the planet vector.


  if (target.isMoon())
  {
    SpiceDouble planet[6];
    memset(planet, 0, sizeof(planet));

    spkez_c ( target.centerPlanet().naifId(),
              epoch.ephemeris(),
              referenceFrame,
              "LT+S",
              instrument.spacecraft().naifId(),
              planet,
              &lightTime );
    ads_checkForError();
      
    memcpy(theGeometry.planetVector, planet,
           sizeof(theGeometry.planetVector));      
  }


  // Find the solar vector.

  SpiceDouble sun[6];
  memset(sun, 0, sizeof(sun));

  spkez_c ( ads_Body::sun().naifId(),
            epoch.ephemeris(),
            referenceFrame,
            "LT+S",
            instrument.spacecraft().naifId(),
            sun,
            &lightTime );
  ads_checkForError();
  
  memcpy(theGeometry.solarVector, sun,
         sizeof(theGeometry.solarVector));      


  // Find the target orientation.

  pxform_c ( referenceFrame,
             target.frameName().c_str(),
             epoch.ephemeris(),
             theGeometry.targetOrientation );
  ads_checkForError();
	 
}
          


