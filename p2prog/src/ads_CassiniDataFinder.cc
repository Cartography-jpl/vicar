// ads_CassiniDataFinder.cc
//
//  April 9, 2002
//  Michael Brady

#include "ads_CassiniDataFinder.h"

#include "ads_TraceAdder.h"
#include "ads_error.h"
#include "ads_Rotate180GeometryFinder.h"
#include "ads_FixedInstGeometryFinder.h"
#include "ads_DefaultGeometryFinder.h"

#include "SpiceUsr.h"

ads_CassiniDataFinder::ads_CassiniDataFinder()
{
   ADS_TRACE;

   // Create a geometry finder which will find the pointing
   // of the spacecraft, then rotate it to the instrument pointing,
   // then rotate it 180 degrees.

   m_geometryFinder.reset( new ads_Rotate180GeometryFinder ( 
                             new ads_FixedInstGeometryFinder ( 
                               new ads_DefaultGeometryFinder() ) ) );
}

ads_CassiniDataFinder::~ads_CassiniDataFinder()
{
}
     

void 
ads_CassiniDataFinder::getGeometryNoUpdates ( int instNaifId,
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

   m_geometryFinder->getGeometry ( instNaifId,
                                   targetNaifId,
                                   utcTime,
                                   referenceFrame,
                                   tolerance,
                                   theGeometry,
                                   metaData );

   // Cassini images need to be rotated 180 degrees.

   Matrix33 rot180;
   memset(rot180, 0, sizeof(rot180));

   rotate_c (pi_c(), 3, rot180);
   ads_checkForError();

   mxm_c(rot180,
          theGeometry.instrumentPointing, 
          theGeometry.instrumentPointing);
   ads_checkForError();
}
