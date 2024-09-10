// ads_getcamcon.cc
//
// Apr 22, 2002
// Michael Brady

#include "ads_getcamcon.h"

#include "ms_defines.h"

#include <string.h>

void ads_getcamcon( const char* miplMissionName,
                    int miplCameraId,
                    double focalLength,
                    double lineObjectSpace,
                    double sampleObjectSpace,
                    double scale )
{
   // Since zgetcamcon, wants a non-const string, copy to a local
   // non-const buffer.

   char projectBuf[128];
   memset(projectBuf, '\0', sizeof(projectBuf));
   strncpy(projectBuf, miplMissionName, sizeof(projectBuf));

   float tFocalLength = 0.0;
   float tLineObjectSpace = 0.0;
   float tSampleObjectSpace = 0.0;
   float tScale = 0.0; 

   int status = 0;
   zgetcamcon(projectBuf,
              miplCameraId,
              &tFocalLength,
              &tLineObjectSpace,
              &tSampleObjectSpace,
              &tScale,
              &status);

   bool wasFound = (status == 0);
 
   if (!wasFound)
   {
      printf("ads_getcamcon.cc:ads_getcamcon:  "
             "Programming error: Camera information not found.  "
             "This statement should not be reachable.\n");
      exit(1);
   }

   focalLength = tFocalLength;
   lineObjectSpace = tLineObjectSpace;
   sampleObjectSpace = tSampleObjectSpace;
   scale = tScale;
}
