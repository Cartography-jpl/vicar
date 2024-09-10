// ads_Rotate180GeometryFinder.cc
//
//  April 9, 2002
//  Michael Brady

#include "ads_Rotate180GeometryFinder.h"

#include "ads_TraceAdder.h"
#include "ads_error.h"
#include "SpiceUsr.h"

ads_Rotate180GeometryFinder::
ads_Rotate180GeometryFinder(ads_GeometryFinder* finder)
   : m_finder(finder)
{
}

ads_Rotate180GeometryFinder::~ads_Rotate180GeometryFinder()
{
}
     

void ads_Rotate180GeometryFinder::getGeometry ( int instNaifId,
                                          int targetNaifId,
                                          const char* utcTime,
                                          const char* referenceFrame,
                                          double tolerance,
                                          Geometry_out theGeometry,
                                          GeometryMetaData_out metaData)
   throw ( SpiceLib::ToolkitException,
           MiplSpiceLib::PointingNotFound )
{
   ADS_TRACE;

   m_finder->getGeometry ( instNaifId,
                           targetNaifId,
                           utcTime,
                           referenceFrame,
                           tolerance,
                           theGeometry,
                           metaData );

   // Rotate 180 degrees.

   Matrix33 rot180;
   memset(rot180, 0, sizeof(rot180));

   rotate_c (pi_c(), 3, rot180);
   ads_checkForError();

   mxm_c(rot180,
          theGeometry.instrumentPointing, 
          theGeometry.instrumentPointing);
   ads_checkForError();
}
