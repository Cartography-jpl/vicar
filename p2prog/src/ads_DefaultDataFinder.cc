// ads_DefaultDataFinder.cc
//
//  April 9, 2002
//  Michael Brady

#include "ads_DefaultDataFinder.h"

#include "ads_DefaultGeometryFinder.h"
#include "ads_TraceAdder.h"
#include "ads_error.h"
#include "ads_getcamcon.h"
#include "ads_DataBase.h"

#include "adc_Instrument.h"

#include "SpiceUsr.h"

ads_DefaultDataFinder::ads_DefaultDataFinder()
{
}

ads_DefaultDataFinder::~ads_DefaultDataFinder()
{
}
     

void 
ads_DefaultDataFinder::
getGeometryNoUpdates ( int instNaifId,
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

   ads_DefaultGeometryFinder().getGeometry ( instNaifId,
                                             targetNaifId,
                                             utcTime,
                                             referenceFrame,
                                             tolerance,
                                             theGeometry,
                                             metaData );
}


void 
ads_DefaultDataFinder::
getGeometry ( const ImageData & image,
              const GeometryMetaData & criteria,
              Geometry_out theGeometry,
              GeometryMetaData_out metaData )
   throw ( SpiceLib::ToolkitException,
           MiplSpiceLib::PointingNotFound )
{
   ADS_TRACE;

   bool wasFoundinDataBase = false;

   // A facility of "NAIF" means the user doesn't want to look
   // at any updates, but only wants the original data.

   if (strcmp(criteria.facility, "NAIF") != 0)
   {
      wasFoundinDataBase = ads_DataBase::findGeometry(image, 
                                                      criteria, 
                                                      theGeometry, 
                                                      metaData);
   }

   if (!wasFoundinDataBase)
   {
      // If we can't find it in the data base, just get it from the kernels.

      adc_Instrument inst(image.inst);
      getGeometryNoUpdates(inst.naifId(),
                           image.target,
                           image.utcTime,
                           image.referenceFrame,
                           image.tolerance,
                           theGeometry,
                           metaData);
   }
}

void 
ads_DefaultDataFinder::
storeGeometry (
      const jpl::mipl::spice::corba::ImageData & image,
      const jpl::mipl::spice::corba::Geometry & theGeometry,
      const jpl::mipl::spice::corba::GeometryMetaData & theMetaData
      )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException)
{
   ADS_TRACE;

   ads_DataBase::storeGeometry(image, theGeometry, theMetaData);
}
          
void 
ads_DefaultDataFinder::getInstrument (const ImageData & image,
                                      InstrumentData_out theInstrument )
   throw ( SpiceLib::ToolkitException )
{
   ADS_TRACE;

   adc_Instrument inst(image.inst);

   double lineObjectSpace;  // This value is ignored.
   double sampleObjectSpace; // This value is ignored.
   ads_getcamcon( inst.miplMissionName(),
                  inst.miplId(),
                  theInstrument.focalLength,
                  lineObjectSpace,
                  sampleObjectSpace,
                  theInstrument.pictureScale );


   theInstrument.cameraBoresightOffset = -1.0;  // TODO implement
   theInstrument.opticalAxisOffset = -1.0;  // TODO implement
}
          
void ads_DefaultDataFinder::getTarget ( const ImageData & image,
                                        Target_out theTarget )
   throw ( SpiceLib::ToolkitException )
{
   ADS_TRACE;

   SpiceInt numValues = 0;
   bodvar_c ( image.target,
              "RADII",
              &numValues,
              theTarget.radii );
   ads_checkForError();
 
 
   // TODO figure out how to find rotation rate.
}
