# coding:utf-8 
'''
Created on 2015年8月16日

@author: taoqu_000
'''
import struct, os

defformat = "h"  # Default data format (2-byte signed short int)

def write3DDat(box, filename, sensor, res, pixel_easting, pixel_northing, bansNames,
               writehdr=True, dataformat=defformat, interleave="bsq"):
    '''
    # Writes a BIL or BSQ file from a 3D list with 1st dimension band number, 2nd dimension line number
    # and 3rd dimension pixel number (unfolds to a 1D array then calls writeDataFile)
    #
    # Arguments:
    # box: 3d array
    # filename: Name of file to be written to
    # writehdr: Flag denoting whether to write an ENVI header file (default true)
    # dataformat: Format string for data, as Python struct definition
    # interleave: "bil" or "bsq" appropriately
    '''
    # Store numbers of bands, lines and pixels per line for convenience
    numbands, numlines, pixperline = box.shape
    
    print('Write ENVI Dat: \n%s' % filename)
    fdir = os.path.dirname(filename)
    if not os.path.isdir(fdir):
        os.makedirs(fdir)
#   右侧合并3d box,到13通道就内存溢出了，windows上暂不可行     
#     datjoin = box[0]
#     for i in range(1, len(box)):
#         datjoin = np.concatenate((datjoin, box[i]), axis=1)

    # Open the data file for writing in binary mode
    try:
        fp = open(filename, 'wb')
    except:
        print ("Could not open data file " + str(filename) + " for writing")
        raise
    
    for i in range(len(box)):
        box[i].astype('int16').tofile(fp)

    fp.close()
    
    # Write header file if requested
    if (writehdr):
        try:
            # Check ENVI data type
            datatype = getEnviType(dataformat)
        except:
#             datafile.close()
            print("Unable to generate header for type " + dataformat + ", data type is not valid for ENVI")

        
        writeHdrFile(filename.replace('.dat', '.hdr'), sensor, pixperline, numlines, numbands,
                     datatype, res, pixel_easting, pixel_northing, bansNames, interleave)


# Function getEnviType
# Gets the ENVI type code equivalent to a particular Python struct format string
#
# Arguments
# formatstr: Struct format string to get ENVI type code for
#
# Returns: ENVI numeric type code for supplied format string
def getEnviType(formatstr):
    
    dtype = -1
    
    # Check the given format string is valid
    try:
        struct.calcsize(formatstr)
    except:
        raise ValueError(formatstr + " is not a valid format string")
    # end try
    
    # Do the conversion
    if (formatstr == "b"):
        dtype = 1  # Signed (?) byte
    elif (formatstr == "h"):
        dtype = 2  # 2-byte signed short int (ENVI calls it an int)
    elif (formatstr == "H"):
        dtype = 12  # 2-byte unsigned int (ENVI calls it an int)
    elif (formatstr == "i"):
        dtype = 3  # 4-byte signed int (ENVI calls it a Long)
    elif (formatstr == "I"):
        dtype = 13  # 4-byte unsigned int (ENVI calls it a Long)
    elif (formatstr == "f"):
        dtype = 4  # 4-byte float
    elif (formatstr == "d"):
        dtype = 5  # 8-byte double precision
    elif (formatstr == "l"):
        dtype = 14  # 8-byte long int (ENVI 64-bit int)
    elif (formatstr == "L"):
        dtype = 15  # 8-byte unsigned long int (ENVI 64-bit int)
    else:
        # If we get here then the format string is valid for Python but not for ENVI, raise an error
        raise ValueError(formatstr + " is a valid Python format string but does not have an ENVI equivalent")
    # end if
    
    return dtype
# end function

# Function writeHdrFile
# Writes an ENVI .hdr file to be associated with a data file
#
# Arguments:
# filename: Name of .hdr file to be written
# samples: Number of pixels per line (samples)
# lines: Number of lines
# bands: Number of bands
# datatype: Numeric code for relevant data type
def writeHdrFile(filename, sensor, samples, lines, bands, datatype, res,
                 pixel_easting, pixel_northing, bansNames, interleave="bsq"):
    try:
        hdrfile = open(filename, "w")
    except:
        print ("Could not open header file " + str(filename) + " for writing")
        raise
    # end try
    
    hdrfile.write("ENVI\n")
    hdrfile.write("description = { HUABEI }\n")
    hdrfile.write("samples = " + str(samples) + "\n")
    hdrfile.write("lines   = " + str(lines) + "\n")
    hdrfile.write("bands   = " + str(bands) + "\n")
    hdrfile.write("header offset = 0\n")
    hdrfile.write("file type = ENVI Standard\n")
    hdrfile.write("data type = " + str(datatype) + "\n")
    hdrfile.write("interleave = " + interleave + "\n")
    hdrfile.write("sensor type = " + sensor + "\n")
    hdrfile.write("byte order = 0\n")
    # Krassovsky
    mapInfo = 'Albers105, 1.0000, 1.0000, %.3f, %.3f, %.10e, %.10e, , units=Meters'\
              % (pixel_easting, pixel_northing, res, res)
    projectionInfo = \
    '9, 6378245.0, 6356863.0, 0.000000, 105.000000, 0.0, 0.0, 25.000000, 47.000000, Albers105, units=Meters'
#    projection info = {31, 6378270.0, 6356794.0, 70.000000, 315.000000, 0.0, 0.0, WGS-84, UPS_North, units=Meters}
    hdrfile.write("map info = {%s}\n" % mapInfo)
    hdrfile.write("projection info = {%s}\n" % projectionInfo)  
    hdrfile.write("band names = {\n")
    hdrfile.write(', '.join(bansNames) + '\n')
    hdrfile.write("}\n")

    hdrfile.flush()
    hdrfile.close()
# end function



if __name__ == '__main__':
    pass
