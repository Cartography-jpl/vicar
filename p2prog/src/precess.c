/* in_ra & in_dec in degrees
   epoch in decimal years like 1950.0
   out_ra & out_dec in degrees J2000
*/
#include <math.h>
#define PI 3.141592653589793238
extern void
precess(in_ra,in_dec,epoch,out_ra,out_dec)
double in_ra,in_dec,epoch;
double *out_ra,*out_dec;
{
 double T,zeta_A,z_A,theta_A,az,de,th,A,B,C;
 double alpha2000,delta2000,alpha_in,delta_in;

 T = (epoch - 2000.0)/100.0;
 zeta_A  = 0.6406161* T + 0.0000839* T*T + 0.0000050* T*T*T;
 z_A     = 0.6406161* T + 0.0003041* T*T + 0.0000051* T*T*T;
 theta_A = 0.5567530* T - 0.0001185* T*T + 0.0000116* T*T*T;
 
 alpha_in=in_ra;
 delta_in=in_dec;
 az=(alpha_in - z_A)*PI/180.;
 de=(delta_in)*PI/180.;
 th=(theta_A)*PI/180;
 A = sin(az) * cos(de);
 B = cos(az) * cos(th) * cos(de) + sin(th) * sin(de);
 C = -cos(az) * sin(th) * cos(de) + cos(th) * sin(de);
 
 alpha2000 = atan2(A,B);
 alpha2000 = alpha2000*180./PI - zeta_A;

 if(alpha2000 < 0.0) alpha2000 = 360.+alpha2000;
 if(alpha2000 > 360.) alpha2000 = alpha2000-360.;

 delta2000 = asin(C);
 delta2000 = delta2000*180./PI;
 *out_ra=alpha2000;
 *out_dec=delta2000; 

}
