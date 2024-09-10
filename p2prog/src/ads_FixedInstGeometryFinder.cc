// ads_FixedInstGeometryFinder.cc
//
//  April 9, 2002
//  Michael Brady


#include "ads_FixedInstGeometryFinder.h"

#include "ads_Body.h"
#include "ads_Time.h"
#include "ads_error.h"
#include "ads_TraceAdder.h"

ads_FixedInstGeometryFinder::
ads_FixedInstGeometryFinder(ads_GeometryFinder* finder)
   : m_finder(finder)
{
}

ads_FixedInstGeometryFinder::~ads_FixedInstGeometryFinder()
{
}

void 
ads_FixedInstGeometryFinder::getGeometry ( int instNaifId,
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

   // First, get the data for the spacecraft frame.

   ads_Body instrument(instNaifId);
   ads_Frame spacecraft = instrument.spacecraft().scFrame();

   int scFrameNaifId = spacecraft.naifId();

   m_finder->getGeometry(scFrameNaifId,
                         targetNaifId,
                         utcTime,
                         referenceFrame,
                         tolerance,
                         theGeometry, 
                         metaData);

   // Now rotate it to the instrument frame.

   ads_Time epoch(utcTime);
   Matrix33 sc2inst;
   memset(sc2inst, 0, sizeof(sc2inst));

   pxform_c (spacecraft.name().c_str(),
             instrument.name().c_str(),
             epoch.ephemeris(),
             sc2inst);
   ads_checkForError();
             
   mxm_c (sc2inst, 
          theGeometry.instrumentPointing, 
          theGeometry.instrumentPointing);
   ads_checkForError();
}   
