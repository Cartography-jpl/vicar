/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* structure for FITS header information */
typedef struct {
    /*
     * plate directory information
     */
    char plate_root[80];    /* directory root for plates                */
    char header_dir[80];    /* directory root for header files          */
    char compression[7];    /* compression to use (low, medium, high)   */
    char plate_name[7];     /* name of this plate                       */
    /*
     * plate id, region identification
     */
    char plate_id[4], region_no[6];
    /*
     * size of section and corner position 
     */
    int section_x_length, section_y_length;
    float section_x_corner, section_y_corner;
    /*
     * target position
     */
    int ra_h, ra_m, dec_d, dec_m;
    float ra_s, dec_s;
    char dec_sign;
    float x, y;
    /*
     * plate solution parameters
     */
    double plt_scale, x_pixel_size, y_pixel_size,
        plt_center_vec[3], ppo_coeff[6],
        amd_x_coeff[20], amd_y_coeff[20],
        plt_center_ra, plt_center_dec;
    char amd_flag, ppo_flag, plate_data_flag,
        special_plate_flag;
    /*
     * miscellaneous information from header keywords
     */
    int plt_grade, emulsion_code;
    float scaling, mag_limit, plt_date;
    char object_name[19], exposure_time[19], seeing[4], origin[19],
        plate_label[7];
    /*
     * header lines
     */
    char hlines[200][81];
    int nhlines;
} Header;
