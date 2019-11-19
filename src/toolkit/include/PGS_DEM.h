/*******************************************************
PGS_DEM.h--

this is the header file for all the PGS_DEM tools. At some point in the future
this may be split up into a series of header files.


In the code for the PGS_DEM tools, there are a series of data "groupings" which
are used.  There are presently four fundamental data groups: datasets, subsets,
subgrids, and files.
  
datasets -- this is the largest scale division of the DEM data.  A particular
data set includes all the information, across all masks, of a particular
resolution.  

subgrid -- is all the data which covers a particular, pre-defined and resolution
specific geographic area.  For example, for the 3 arc secod resolution a
subgrid would be all the files which cover one 10 degree by 10 degree
section of the world.

subset -- is all the data of groups of particular masks.  A subgrid divides the
dataset into geographic sections, the subset divides it by groups of data
attributes.  For example, one subset could be the data for elevation, slope and
slope aspect and another subset could be the data for standard deviation of
slope, standard deviation of elevation, and land/water mask.

file -- is an individual UNIX file.  For this tool we have used the HDF-EOS file
format. a file is actually the intersection of a particular subgrid with a
particular subset.  

   An example:
        for the 3 arc second resolution of the world one would have:
	1 dataset of 3 arc second data.
	648 subgrids of 10 degree by 10 degree geographic sections of the world.
	3 subsets, each having some 'subset' of the 8 attributes or masks.
	1944 individual files.



Author -- Alexis Zubrow
Note: I would like to thank Guru Tej Khalsa for his invaluable assistance in
design. 

history --
January 21st, 1997   AZ  first created
July 31, 1997        AZ  Added bilinear interpolation functions, changed 
                         quality flags
September, 10 Abe Taaheri added land/sea mask idntifiers
June, 5, 2000 Abe Taaheri added tags for 3km resolution

*******************************************************/


/* general include files for error handling and types for SDP Toolkit */
#include <PGS_SMF.h>

#ifndef PGSd_DEM_PROTOS_ONLY

#include <PGS_PC.h>     /*include for PC #defines */
#include <mfhdf.h>      /*include for HDF #defines*/
#include <PGS_DEM_14.h> /*include for generated error messages*/
#include <HdfEosDef.h>  /*include for HDF-EOS #defines */

/* Attribute Flags -- "masks" --Eventually figure out a better numbering system,
 used for keeping track of which layers have been initialized. Need to have
 numbers which when summed keep a record of the layers initialized*/

#define  PGSd_DEM_ELEV        1 /*Elevation*/
#define  PGSd_DEM_STDEV_ELEV  10 /*Standard Deviation, Elevation */
#define  PGSd_DEM_SLOPE       100 /*Slope Gradient*/
#define  PGSd_DEM_STDEV_SLOPE 1000 /*Standard Deviation, Slope Gradient*/
#define  PGSd_DEM_ASPECT      10000 /*Slope Aspect*/
#define  PGSd_DEM_TOP_OBSC    100000 /*Topological Obscuration*/
#define  PGSd_DEM_TOP_SHAD    1000000 /*Topological Shadow*/
#define  PGSd_DEM_WATER_LAND  10000000 /*Water/Land mask*/

/*Maximum number of layers recognized by the tools.   This should be equal to
  the number of layers recorded above.*/
#define PGSd_DEM_MAX_LAYERS   8


/* Quality/Source layer flags */
#define  PGSd_DEM_SOURCE      5  /*Data Source*/
#define  PGSd_DEM_METHOD      15 /*Method of accuracy calculation*/
#define  PGSd_DEM_GEOID       25 /*Geoid data*/
#define  PGSd_DEM_VERTICAL_ACCURACY       35  /*Vertical Accuracy data*/
#define  PGSd_DEM_HORIZONTAL_ACCURACY     45  /*Horizontal Accuracy data*/

/* Identifiers for the different Resolutions--  */
#define  PGSd_DEM_3ARC         1 /*3 arc second data set,~100m postings*/
#define  PGSd_DEM_30ARC        2 /*30 arc second data set, ~1km postings*/
#define  PGSd_DEM_90ARC        4 /*90 arc second data set, ~3km postings*/
#define  PGSd_DEM_30TEST       3 /*30 arc second, but 40 by 50 degree sections*/

/* Land/Sea mask identifiers 
The mask DN values are 0-7 and refer to:
 
 0. Shallow Ocean (Ocean <5k from coast OR <50m deep; i.e., a
    buffer zone around all coastal areas and islands, plus shallow
    areas up to 50m deep that are further than 5km from the land).
    Includes the appropriate parts of the Black Sea, Red Sea,
    Mediterranean Sea, Hudson Bay, and other ocean-connected seas.
 
 1. Land (not anything else).
 
 2. Ocean Coastlines and Lake Shorelines (an actual boundary
    line).
 
 3. Shallow Inland Water (Inland Water <5km from shore OR <50m
    deep; i.e., a buffer zone around all lake shores and inland
    islands, plus shallow areas up to 50m deep that are further than
    5km from the land). Includes the appropriate parts of the Caspian
    Sea, Aral Sea, Great Lakes, "2-line" rivers, etc.
 
 4. Ephemeral (intermittent) Water (from Digital Chart of the World).
 
 5. Deep Inland Water (Inland water >5km from shoreline AND >50m
    deep; i.e., Lake waters beyond 5km from their shore or islands,
    and greater than 50m deep). Includes the appropriate parts of the
    Caspian Sea, Aral Sea, Great Lakes, etc.
 
 6. Moderate or Continental Ocean (Ocean >5km from coast AND >50m deep 
    but <500mdeep); i.e., Oceans beyond 5km from coastal areas and islands, 
    and greater than 50m deep but less than 500m deep). Includes the 
    appropriate parts of the Black Sea, Red Sea, Mediterranean Sea, 
    Hudson Bay, and other ocean-connected seas.
 
 7. Deep Ocean (Ocean >500m deep), includes most of the world oceans.
 */ 
#define  PGS_DEM_SHALLOW_OCEAN             0
#define  PGS_DEM_LAND                      1
#define  PGS_DEM_SHORELINE                 2
#define  PGS_DEM_SHALLOW_INLAND_WATER      3
#define  PGS_DEM_EPHEMERAL_WATER           4
#define  PGS_DEM_DEEP_INLAND_WATER         5
#define  PGS_DEM_MODERATE_OCEAN            6
#define  PGS_DEM_DEEP_OCEAN                7

/* Incomplet data flag */
#define  PGSd_DEM_NO_COMPLETE_DATA -3 /*None of the data sets queried have
					complete data*/

/* Interpolation Method */
#define  PGSd_DEM_NEAREST_NEIGHBOR  1 /*Nearest Neighbor interpolation */
#define  PGSd_DEM_BILINEAR          2 /*Bilinear interpolation */


/* geopositioning format-- either decimal degrees or global pixels*/
#define PGSd_DEM_PIXEL      11     /*pixels*/
#define PGSd_DEM_DEGREE     22     /*Decimal degrees*/


/*maximum number of resolutions that can be in resolutionList*/
#define PGSd_DEM_MAX_RESOLUTIONS   3

#endif

/* typedef the resolution handle */
typedef PGSt_integer PGSt_DEM_Tag;

/* Function Prototypes (public routines) */

#ifdef __cplusplus
extern "C" {
#endif

PGSt_SMF_status 
PGS_DEM_Open(
    PGSt_DEM_Tag [], 
    PGSt_integer,
    PGSt_integer [],
    PGSt_integer);

PGSt_SMF_status 
PGS_DEM_Close(
    PGSt_DEM_Tag [], 
    PGSt_integer,
    PGSt_integer [],
    PGSt_integer);

PGSt_SMF_status
PGS_DEM_DataPresent(
    PGSt_DEM_Tag,
    PGSt_integer,
    PGSt_integer,
    PGSt_double [],
    PGSt_double [],
    PGSt_integer,
    PGSt_boolean*);

PGSt_SMF_status
PGS_DEM_SortModels(
    PGSt_DEM_Tag [],
    PGSt_integer,
    PGSt_integer,
    PGSt_integer,
    PGSt_double [2],
    PGSt_double [2],
    PGSt_DEM_Tag *);

PGSt_SMF_status
PGS_DEM_GetPoint(
    PGSt_DEM_Tag [],
    PGSt_integer,
    PGSt_integer,
    PGSt_integer,
    PGSt_double [],
    PGSt_double [],
    PGSt_integer,
    PGSt_integer,
    void*);

PGSt_SMF_status
PGS_DEM_GetRegion(
    PGSt_DEM_Tag [],
    PGSt_integer,
    PGSt_integer,
    PGSt_integer,
    PGSt_integer,
    PGSt_double [2],
    PGSt_double [2],
    void*,
    PGSt_double [2],
    PGSt_double [2],
    PGSt_double [2]);

PGSt_SMF_status
PGS_DEM_GetMetadata(
    PGSt_DEM_Tag,
    PGSt_integer,
    PGSt_double [2],
    PGSt_double [2],
    char*,
    PGSt_double*,
    PGSt_double*,
    PGSt_double*,
    char*,
    PGSt_integer*,
    PGSt_boolean*);

PGSt_SMF_status
PGS_DEM_GetQualityData(
    PGSt_DEM_Tag,
    PGSt_integer,
    PGSt_integer,
    PGSt_double [2],
    PGSt_double [2],
    void*);

PGSt_SMF_status
PGS_DEM_GetSize(
    PGSt_DEM_Tag,
    PGSt_integer,
    PGSt_integer,
    PGSt_double [2],
    PGSt_double [2],
    PGSt_integer*,
    PGSt_integer*,
    PGSt_integer*);


/****************************************************************/

#ifndef PGSd_DEM_PROTOS_ONLY

/*Internal (private) typedefs, prototypes, #defines */

/* Subset codes-- actually logical ID's from PCF */
#define  PGSd_DEM_SUBSET30A     10650  /*One of 30 arc second subsets*/
#define  PGSd_DEM_SUBSET30B     10651  /*One of 30 arc second subsets*/
#define  PGSd_DEM_SUBSET30C     10652  /*One of 30 arc second subsets*/
#define  PGSd_DEM_SUBSET3A     10653  /*One of 3 arc second subsets*/
#define  PGSd_DEM_SUBSET3B     10654  /*One of 3 arc second subsets*/
#define  PGSd_DEM_SUBSET3C     10655  /*One of 3 arc second subsets*/
#define  PGSd_DEM_SUBSET90A     10656  /*One of 90 arc second subsets*/
#define  PGSd_DEM_SUBSET90B     10657  /*One of 90 arc second subsets*/
#define  PGSd_DEM_SUBSET90C     10658  /*One of 90 arc second subsets*/
#define  PGSd_DEM_SUBSET30TEST  10649  /*30 arc second test subset*/

/*add more logical ID's as more resolutions and subsets become available*/

/*Flag indicating no error in opening a resolution or layer*/
#define  PGSd_DEM_NO_ERROR         -3  


/* Fill values. Important to check that no fill value is unique in particular
   layer. */

#define  PGSd_DEM_NO_FILLVALUE     -8888 /*No fill value for that attribute */
#define  PGSd_DEM_FILLVALUE        -9999 /*Presently all the layers (i.e. masks)
					   have the same fillvalue, to change
					   this, need to change
					   PGS_DEM_Lookup.c*/
#define  PGSd_DEM_NODATA           -7777 /* NO real data available */
/*flags indicating a point has data or is fill value, used in _RecursiveSearch
  functions*/
#define  PGSd_DEM_FILLPOINT        -11  /*Point is fill value*/
#define  PGSd_DEM_DATAPOINT         11  /*Point has actual data*/

/* Commands for PGS_DEM_Subset function */
#define  PGSd_DEM_INITIALIZE        -1  /*Open (initialize) flag*/
#define  PGSd_DEM_DEINITIALIZE      -2  /*Close (de-initialize) flag*/
#define  PGSd_DEM_INFO              -3  /*Get info on subset*/
#define  PGSd_DEM_QUALITYINFO       -4  /*Get quality/source/geoid data info*/
#define  PGSd_DEM_QUALITYCLOSE      -5  /*Separate close for quality/... data,
					  used only within PGS_DEM_GetSize and 
					  PGS_DEM_GetQualityData*/


/* Flags for layers and source/quality data */
#define  PGSd_DEM_EXISTS   33   /*Subset,Layer or Source/Quality exists */
#define  PGSd_DEM_ABSENT  -33    /*Subset, Layer, Source/Quality not included*/


/*If no subgrids have yet been accessed in a resolution, then it is assigned
  this value as a flag for entering a new resolution-- used in
  PGS_DEM_RecursiveSearch()*/
#define  PGSd_DEM_NOT_ACCESSED  -1


/* Initialize hdfId and gdID, HDF-EOS handles, to indicate that they have not
   been opened or attached.*/
#define  PGSd_DEM_HDF_CLOSE      0

/*Initialize subgrid value to indicate that the subgrid has not yet been
  accessed*/
#define  PGSd_DEM_SUBGRID_CLOSE   -4
#define  PGSd_DEM_SUBGRID_EXIST   -3

/* Size of string for attributes retrieved by PGS_MET tools.  I picked this as a
   maximum size for the attributes retrieved. See PGS_DEM_GetMetadata to see
   particular attributes of interest*/
#define  PGSd_DEM_STRING_MET     200


/* Commands for PGS_DEM_AccessFile function */
#define  PGSd_DEM_ASSIGN         11    /*Assign flag*/
#define  PGSd_DEM_OPEN           13    /*Open (initialize) flag*/
#define  PGSd_DEM_CLOSE          17    /*Close flag*/


/* Maximum number of files which can simultaneously be staged.  This is
   dependent on the maximum number of HDF-EOS GRIDS which can simultaneosly be
   open, see HDF-EOS header files (esp. GDhdfeos.h). If the value of NGRID
   changes, one could change this value correspondingly*/
#define PGSd_DEM_MAX_STAGED      200


/*define structure used for quick access for information about different 
  subsets*/
typedef struct
{
  PGSt_PC_Logical subset;          /*Subset value, logical ID of subset*/
  PGSt_DEM_Tag resolutionTag;      /*Resolution- particular dataset*/
  PGSt_integer numSubgrids;        /*Number of subgrids for part. resolution*/
  PGSt_integer versionLength;      /*Number of file staged in PCF*/
  PGSt_integer firstSubgrid;       /*Number of first subgrid staged*/
  PGSt_integer horizPixSubgrid;    /*Number horizontal pixels in a subgrid*/
  PGSt_integer vertPixSubgrid;     /*Number vertical pixels in a subgrid*/
  PGSt_integer subgridVert;        /*Number subgrids vert. spanning world*/ 
  PGSt_integer subgridHoriz;       /*Number subgrids horiz. spanning world*/
  PGSt_integer pixPerDegree;       /*Number pixels per degree (lat. or lon.)*/
  PGSt_integer layer;              /*Layer (i.e. mask) to access data from*/
  PGSt_integer layersInit;         /*Layers initialized, increment each layer*/
  PGSt_integer fillvalue;          /*Fill value for the layer (mask)*/
  PGSt_integer numBytes;           /*Size of data pixel from particular layer*/
  PGSt_integer dataType;           /*type of data-- int, double,. see HDF*/
  PGSt_double offset;              /*pixel position offset from center*/
  char gridName[VSNAMELENMAX];     /*Name of grid in which the layer resides*/
  char fieldName[VSNAMELENMAX];    /*Name of field corresponding to layer*/
}PGSt_DEM_SubsetRecord;


/* define structure used for quick access to individual file specific 
information */
typedef struct
{
  char filePath[PGSd_PC_FILE_PATH_MAX]; /*full name of file, including path*/
  PGSt_integer version;            /*version number from PCF*/
  PGSt_integer subgrid;            /*Subgrid number*/
  PGSt_integer cornerRow[2];       /*Row values of top (0th element) & bottom*/
  PGSt_integer cornerCol[2];       /*Columnn values,left (0th element) & right*/
  PGSt_double cornerLat[2];        /*Decimal degrees, top (0th elem.) & bottom*/
  PGSt_double cornerLon[2];        /*Decimal degrees, left (0th elem.) & right*/
  PGSt_integer pntsRequested;      /*Number points to get, see RecursiveSearch*/
  int32 hdfID;                     /*File tag, return from GDopen*/
  int32 gdID;                      /*GRID tag, return from GDattach*/
}PGSt_DEM_FileRecord;

/*union to hold various sized data elements*/
typedef union
{
  int8 OneByteInt;
  int16 TwoByteInt;
  float32 FourByteFlt;
} PGSt_DEM_DataStorage;

/*union to hold various sized data elements*/
typedef union
{
  int8 *pntOneByteInt;
  int16 *pntTwoByteInt;
  float32 *pntFourByteFlt;
} PGSt_DEM_DataBuffer;

/*union to hold various position elements-- ie. position in decimal degrees and
  pixels, the first element of the arrays is latitude or row, the second is
  longitude or column*/
typedef union
{
  PGSt_integer pixels[2];  		/*Position in pixels*/
  PGSt_double decimalDegree[2];		/*Position in decimal degrees*/
} PGSt_DEM_Position;

/*stuct to hold point data and the important ancillary data for pixel calls, the
  first element of the arrays is latitude or row, the second is longitude or 
  column*/
typedef  struct {
  PGSt_integer subgridValue;      /*subgrid value of lat, long*/
  PGSt_DEM_Tag resolutionOrig;	  /*original resolution*/
  PGSt_integer positionOrig[2];   /*geoposition in original resolution*/
  PGSt_DEM_DataStorage dataValue;   /*actual data*/
} PGSt_DEM_PointRecordPix;

/*stuct to hold point data and the important ancillary data for decimal degree
  calls*/
typedef  struct {
  PGSt_integer subgridValue;     	  /*subgrid value of lat, long*/
  PGSt_DEM_Tag resolutionOrig;	  /*original resolution*/
  PGSt_double positionOrig[2];    /*geoposition in original resolution*/
  PGSt_DEM_DataStorage dataValue;   /*actual data*/
} PGSt_DEM_PointRecordDeg;

/*struct to hold ancillary information necessary to perform bilinear
  interpolation on a particular point.  When a position is recorded, the first
  element is always latitude.  The bounding array is the 4 nearest points to the
  point to be interpolated. element 0- SW point, element 1- SE point, element 2
  - NE point, element 3-- NW point. */

typedef  struct {
  PGSt_double positionInterp[2];     /*position of pnt. to be interpolated*/
  PGSt_double boundingPnts[4][2];    /*Array of bounding pnts*/
  PGSt_integer nearest;              /*nearest neighbor-- element of array*/
} PGSt_DEM_PointRecordBil;


/*struct to hold information on the region to be extracted.  This is the info
  for a subregion, the extent of a region over one subgrid.  The coordinates
  recorded are interms of internal pixels.  These pixel values are valid only
  for positioning within this particular HDF-EOS GRID. The first value in the
  array is for the upper extent of the region or the left most extent of the
  region, depending on if rowExtent or colExtent. */

typedef struct {
  PGSt_integer rowExtent[2];     /*bounding row pixels to be extracted*/
  PGSt_integer colExtent[2];     /*bounding column pixels to be extracted*/
  PGSt_integer sizeSubregion;    /*size buffer needed to hold data, in pixels*/ 
} PGSt_DEM_RegionRecord;

  
  

/*Macros*/  /*READ THIS SECTION before adding new data sets!!!!!*/

/*All of these macros may at one point be converted into functions.  At the
  moment I have left them as macros because of speed.  All of these macros are
  dependent on the fact that the data is a geographic projection (ie. equal
  angle).  If new projections are at some point added, these macros will have to
  be modified and there may be some serious reprograming of the system. All of
  the macros are also dependent on the Ellipsoid (spheroid) and Datum selected
  for the data sets.  Presently the only ellipsoids being used are WGS84 and
  WGS72.  Transformations between these two ellipsoids have minimum distortions
  (approximately 0.5 arc seconds).  If data sets based on other ellipsoids are
  added in the future, the distortions MUST be tested.  If the offsets are great
  enough, more sophisticated transformations must be employed (ie. transforming
  to a common projection and common ellipsoid).*/


/*Calculates subgrid number from global pixel latitude and longitude, subsetInfo
  is a pointer of type PGSt_DEM SubsetRecord*/

#define PGSm_DEM_PixToSubgrid(pixLat, pixLon, subsetInfo) \
       (((PGSt_integer)(pixLon/(subsetInfo -> horizPixSubgrid)))* \
	(subsetInfo -> subgridVert) +\
	((PGSt_integer)(pixLat/(subsetInfo -> vertPixSubgrid))))

	 /*Calculates the subgrid number based on signed decimal degrees
	   latitude and longitude, subsetInfo is a pointer of type
	   PGSt_DEM_SubsetRecord*/

#define PGSm_DEM_LatLonToSubgrid(lat, lon, subsetInfo) \
	 PGSm_DEM_PixToSubgrid(PGSm_DEM_LatToPixel(lat, subsetInfo), \
			       PGSm_DEM_LonToPixel(lon, subsetInfo), \
			       subsetInfo)

	 /*calculates the pixel value from a latitude or longitude in signed
	   decimal degree format. Returns a pixel value of type
	   PGSt_integer. SubsetInfo is a pointer of type PGSt_DEM_SubsetRecord.
	   the value. 0.5 is half a pixel, this is the position of the center
	   point from the edge of the pixel.  subsetInfo -> offset, is the
	   offset, from this center point. this is a patch because the old 3ARC
	   data has pixels corner located and 30ARC has center located
	   pixels*/ /* Note: Currently all reolutions are center located, i.e.
                       offset =0 */

#define PGSm_DEM_LatToPixel(lat, subsetInfo) \
	 (PGSt_integer)(subsetInfo -> offset + ((90.0 - lat) * \
					       (subsetInfo -> pixPerDegree)))

#define PGSm_DEM_LonToPixel(lon, subsetInfo) \
	 (PGSt_integer)(subsetInfo -> offset + ((180.0 + lon) * \
						(subsetInfo -> pixPerDegree)))

	 /*calculates the latitude or longitude in signed decimal degrees from
	   either the row or column pixels, respectively.  It returns a position
	   of type PGSt_double and subsetInfo is a pointer of type
	   PGSt_DEM_SubsetRecord */

#define PGSm_DEM_PixelToLat(row, subsetInfo) \
	 (90.0 - ((PGSt_double)(row + 0.5 - (subsetInfo -> offset)) \
		  / (subsetInfo -> pixPerDegree)))

#define PGSm_DEM_PixelToLon(col, subsetInfo) \
	  (((PGSt_double)(col + 0.5 - (subsetInfo -> offset)) \
	    /(subsetInfo -> pixPerDegree)) - 180.0)
  
	 /*This macros converts pixel position in one resolution to pixel
	   position in another resolution.*/

#define PGSm_DEM_PixToPix(pix, subsetInfoOrig, subsetInfoNext) \
	 (PGSt_integer) \
	 ((PGSt_double)(pix + 0.5 - (subsetInfoOrig -> offset)) * \
	  ((PGSt_double) (subsetInfoNext -> pixPerDegree) \
	   / (subsetInfoOrig -> pixPerDegree)))

									   
/* Prototypes */

PGSt_SMF_status
PGS_DEM_Subset(
    PGSt_DEM_Tag,
    PGSt_integer,
    PGSt_integer,
    PGSt_DEM_FileRecord ***,
    PGSt_DEM_SubsetRecord **);

PGSt_SMF_status
PGS_DEM_Lookup(
    PGSt_DEM_Tag,
    PGSt_integer,
    PGSt_DEM_SubsetRecord *);

PGSt_SMF_status
PGS_DEM_OrderSubset(
    PGSt_DEM_SubsetRecord *,
    PGSt_integer [][2]);

PGSt_SMF_status
PGS_DEM_Populate(
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_FileRecord *,
    PGSt_DEM_FileRecord **,
    PGSt_integer [][2]);


PGSt_SMF_status
PGS_DEM_WriteSubgridCalculator(
    PGSt_DEM_Tag,
    PGSt_double,
    PGSt_double,
    PGSt_integer *,
    PGSt_integer *,
    PGSt_integer *);

PGSt_SMF_status
PGS_DEM_RecursiveSearchPix(
    PGSt_DEM_Tag [],     
    PGSt_integer,        
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_FileRecord  **,
    PGSt_DEM_PointRecordPix **,
    PGSt_integer,
    PGSt_integer [],        
    PGSt_integer);  

PGSt_SMF_status
PGS_DEM_RecursiveSearchDeg(
    PGSt_DEM_Tag [],     
    PGSt_integer,        
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_FileRecord  **,
    PGSt_DEM_PointRecordDeg **,
    PGSt_integer,
    PGSt_integer [],        
    PGSt_integer);  

PGSt_SMF_status
PGS_DEM_RecursiveSearchBil(
    PGSt_DEM_Tag [],     
    PGSt_integer,        
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_FileRecord  **,
    PGSt_DEM_PointRecordDeg **,
    PGSt_integer,
    PGSt_integer [],        
    PGSt_integer);  

PGSt_SMF_status
PGS_DEM_OrderIndices(
    PGSt_integer [],
    PGSt_integer,
    PGSt_DEM_PointRecordPix **);


PGSt_SMF_status
PGS_DEM_OrderIndicesSumPix(
    PGSt_integer [],
    PGSt_integer,
    PGSt_DEM_PointRecordPix **,
    PGSt_DEM_FileRecord **,
    PGSt_integer *);

PGSt_SMF_status
PGS_DEM_OrderIndicesSumDeg(
    PGSt_integer [],
    PGSt_integer,
    PGSt_DEM_PointRecordDeg **,
    PGSt_DEM_FileRecord **,
    PGSt_integer *);

PGSt_SMF_status
PGS_DEM_ExtentRegion(
    PGSt_integer [2],
    PGSt_integer [2],
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_RegionRecord **,
    PGSt_integer *,
    PGSt_integer *);

PGSt_SMF_status
PGS_DEM_ExtractRegion(
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_FileRecord **,
    PGSt_DEM_RegionRecord **,
    void *,
    PGSt_integer,  
    PGSt_integer,
    PGSt_integer);

PGSt_SMF_status
PGS_DEM_ReplaceFillPoints(
    PGSt_DEM_Tag [],
    PGSt_integer,
    PGSt_integer,
    PGSt_integer,
    void *,
    PGSt_DEM_SubsetRecord *,
    PGSt_integer [2],
    PGSt_integer [2]);

PGSt_SMF_status
PGS_DEM_ReplaceFillPointsInt8(
    PGSt_DEM_Tag [],
    PGSt_integer,
    PGSt_integer,
    PGSt_integer,
    void *,
    PGSt_DEM_SubsetRecord *,
    PGSt_integer [2],
    PGSt_integer [2]);


PGSt_SMF_status
PGS_DEM_ReplaceFillPointsInt16(
    PGSt_DEM_Tag [],
    PGSt_integer,
    PGSt_integer,
    PGSt_integer,
    void *,
    PGSt_DEM_SubsetRecord *,
    PGSt_integer [2],
    PGSt_integer [2]);

PGSt_SMF_status
PGS_DEM_ReplaceFillPointsFlt32(
    PGSt_DEM_Tag [],
    PGSt_integer,
    PGSt_integer,
    PGSt_integer,
    void *,
    PGSt_DEM_SubsetRecord *,
    PGSt_integer [2],
    PGSt_integer [2]);

PGSt_SMF_status
PGS_DEM_AccessFile(
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_FileRecord **,
    PGSt_integer,
    PGSt_integer);

PGSt_SMF_status
PGS_DEM_GetBoundingPnts(
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_PointRecordBil *);

PGSt_SMF_status
PGS_DEM_Interpolate(
    PGSt_DEM_SubsetRecord *,
    PGSt_DEM_PointRecordBil *,
    PGSt_double [4],
    PGSt_integer [4],
    PGSt_integer,
    PGSt_double *);


#ifdef __cplusplus
}
#endif

#endif
