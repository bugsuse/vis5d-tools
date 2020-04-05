/* This version of fromxwd has been fixed by Brian Paul to work with */
/* TrueColor xwd files. */

/*
 *	fromxwd -
 *		Convert an xwd file to IRIS image file format.
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *				Paul Haeberli - 1990
 */


/*
 * The following XCOLOR struct is to be used in place of X's XColor
 * struct because on 32-bit systems, sizeof(XColor)=12 while on 64-bit
 * systems, sizeof(XColor)=16.  We MUST have an XColor struct of size
 * 12 to correctly read the xwd file.  BEP July-21-1995
 */
typedef struct {
        unsigned int /*long*/ pixel;
        unsigned short red, green, blue;
        char flags;  /* do_red, do_green, do_blue */
        char pad;
} XCOLOR;


#include <stdio.h>
#include <X11/Xlib.h> 
#include <X11/Xutil.h>
#include <X11/XWDFile.h>
#include "gl/image.h"

unsigned long MyXGetPixel();
extern char *malloc();
unsigned Image_Size();

unsigned short *rmap;
unsigned short *gmap;
unsigned short *bmap;
short rbuf[8192];
short gbuf[8192];
short bbuf[8192];

printxwdheader(xi)
XWDFileHeader *xi;
{
    fprintf(stderr,"header size %d\n",xi->header_size);
    fprintf(stderr,"file version %d\n",xi->file_version);
    fprintf(stderr,"pixmap format %d\n",xi->pixmap_format);
    fprintf(stderr,"pixmap depth %d\n",xi->pixmap_depth);
    fprintf(stderr,"width %d height %d\n",xi->pixmap_width,xi->pixmap_height);
    fprintf(stderr,"xoffset %d\n",xi->xoffset);
    fprintf(stderr,"byte order %d\n",xi->byte_order);
    fprintf(stderr,"bitmap unit %d\n",xi->bitmap_unit);
    fprintf(stderr,"bitmap bit order %d\n",xi->bitmap_bit_order);
    fprintf(stderr,"bitmap pad %d\n",xi->bitmap_pad);
    fprintf(stderr,"bits per pixel %d\n",xi->bits_per_pixel);
    fprintf(stderr,"bytes per line %d\n",xi->bytes_per_line);
    fprintf(stderr,"visual class %d\n",xi->visual_class);
    fprintf(stderr,"red mask %d\n",xi->red_mask);
    fprintf(stderr,"green mask %d\n",xi->green_mask);
    fprintf(stderr,"blue mask %d\n",xi->blue_mask);
    fprintf(stderr,"colormap entries %d\n",xi->colormap_entries);
    fprintf(stderr,"ncolors %d\n",xi->ncolors);
    fprintf(stderr,"window width %d\n",xi->window_width);
    fprintf(stderr,"window height %d\n",xi->window_height);
    fprintf(stderr,"window x %d\n",xi->window_x);
    fprintf(stderr,"window y %d\n",xi->window_y);
    fprintf(stderr,"window border width %d\n",xi->window_bdrwidth);
}

writeimage(name,ximage,rmap,gmap,bmap,visual_class)
char *name;
XImage *ximage;
unsigned short *rmap, *gmap, *bmap;
int visual_class;
{
    IMAGE *image;
    int y, x;
    unsigned long index;
    int xsize, ysize;
    int rshift = 0, gshift = 0, bshift = 0;
    unsigned long rmask, gmask, bmask;
    int rscale, gscale, bscale;

    xsize = ximage->width;
    ysize = ximage->height;
    image = iopen(name,"w",RLE(1),3,xsize,ysize,3);
    
    if (rmask = ximage->red_mask){
	while (!(rmask & 1)) {
	    rmask >>= 1;
	    rshift++;
	}
    }
    if (gmask = ximage->green_mask){
	while (!(gmask & 1)) {
	    gmask >>= 1;
	    gshift++;
	}
    }
    if (bmask = ximage->blue_mask){
	while (!(bmask & 1)) {
	    bmask >>= 1;
	    bshift++;
	}
    }

    /* If the Ximage uses less than 8-bit per component color, then scale */
    /* up to 8-bit components */
    {
       int rmax = ximage->red_mask   >> rshift;  /* max red value */
       int gmax = ximage->green_mask >> gshift;  /* max green value */
       int bmax = ximage->blue_mask  >> bshift;  /* max blue value */
       rscale = 255 / rmax;
       gscale = 255 / gmax;
       bscale = 255 / bmax;
    }

    for(y=0; y<ysize; y++) {
       if ((visual_class == TrueColor) || (visual_class == DirectColor)){
	  /* No lookup table */
	  if (rscale==1 && gscale==1 && bscale==1) {
	     for(x=0; x<xsize; x++) {
		index = MyXGetPixel(ximage,x,y);
		rbuf[x] = (index>>rshift)&rmask;
		gbuf[x] = (index>>gshift)&gmask;
		bbuf[x] = (index>>bshift)&bmask;
	     }
	  }
	  else {
	     for(x=0; x<xsize; x++) {
		index = MyXGetPixel(ximage,x,y);
		rbuf[x] = ((index>>rshift)&rmask) * rscale;
		gbuf[x] = ((index>>gshift)&gmask) * gscale;
		bbuf[x] = ((index>>bshift)&bmask) * bscale;
	     }
	  }
       }
       else {
	  /* Use lookup table */
	  for(x=0; x<xsize; x++) {
	     index = MyXGetPixel(ximage,x,y);
	     rbuf[x] = rmap[index];
	     gbuf[x] = gmap[index];
	     bbuf[x] = bmap[index];
	  }
       }
       putrow(image,rbuf,ysize-1-y,0);
       putrow(image,gbuf,ysize-1-y,1);
       putrow(image,bbuf,ysize-1-y,2);
    }
    iclose(image);
}

main(argc, argv)
int argc;
char **argv;
{
    int index;
    register int i;
    register char *buffer;
    unsigned long swaptest = 1;
    int count;
    unsigned buffer_size;
    int win_name_size;
    int ncolors;
    char *win_name;
    XCOLOR *colors;
    XWDFileHeader hdr;
    XImage in_image;
    FILE *in_file;
    int debug;

    if(argc<3) {
	fprintf(stderr,"usage: fromxwd in.xwd out.rgb [-debug]\n");
	exit(1);
    }
    if(argc>3)
	debug = 1;
    else
	debug = 0;
    in_file = fopen(argv[1], "r");
    if (in_file == NULL) {
	fprintf(stderr,"Can't open input file %s\n",argv[1]);
	exit(1);
    }
    
    /*
     * Read in header information.
     */
    if(fread((char *)&hdr, sizeof(hdr), 1, in_file) != 1)
      Error("Unable to read dump file header.");

    if (*(char *) &swaptest)
	_swaplong((char *) &hdr, sizeof(hdr));

    if(debug)
	printxwdheader(&hdr);

    /* check to see if the dump file is in the proper format */
    if (hdr.file_version != XWD_FILE_VERSION) {
	fprintf(stderr,"xwud: XWD file format version mismatch.");
	Error("exiting.");
    }
    if (hdr.header_size < sizeof(hdr)) {
	fprintf(stderr,"xwud: XWD header size is too small.");
	Error("exiting.");
    }
    /* alloc window name */
    win_name_size = (hdr.header_size - sizeof(hdr));
    if((win_name = malloc((unsigned) win_name_size)) == NULL)
      Error("Can't malloc window name storage.");

     /* read in window name */
    if(fread(win_name, sizeof(char), win_name_size, in_file) != win_name_size)
      Error("Unable to read window name from dump file.");

    if(debug)
	fprintf(stderr,"Win name is %s\n",win_name);

    /* read in the color map buffer */
    ncolors = hdr.ncolors;
    if(ncolors != 0) {
	colors = (XCOLOR *)malloc((unsigned) ncolors * sizeof(XCOLOR));
	if (!colors)
	    Error("Can't malloc color table");
	rmap = (unsigned short *)malloc(ncolors*sizeof(short));
	gmap = (unsigned short *)malloc(ncolors*sizeof(short));
	bmap = (unsigned short *)malloc(ncolors*sizeof(short));
	if(fread((char *) colors, sizeof(XCOLOR), ncolors, in_file) != ncolors)
	  Error("Unable to read color map from dump file.");
	if (*(char *) &swaptest) {
	    for (i = 0; i < ncolors; i++) {
		_swaplong((char *) &colors[i].pixel, sizeof(long));
		_swapshort((char *) &colors[i].red, 3 * sizeof(short));
	    }
	}
	if (hdr.visual_class!=TrueColor) {
	    for (i = 0; i < ncolors; i++) {
	        if(colors[i].pixel>=ncolors) {
		     fprintf(stderr,"pixel value %d is out of range 0..%d\n",
			  colors[i].pixel,ncolors-1);
		     exit(1);
	        }
	        index = colors[i].pixel;
	        rmap[index] = (colors[i].red>>8);
	        gmap[index] = (colors[i].green>>8);
	        bmap[index] = (colors[i].blue>>8);
	        if(debug) {
		    fprintf(stderr,"pixel: %d  ",index);
		    fprintf(stderr,"r: %d g: %d b: %d\n",rmap[index],gmap[index],bmap[index]);
	        }
	    }
	}
    } else {
	rmap = (unsigned short *)malloc(2*sizeof(short));
	gmap = (unsigned short *)malloc(2*sizeof(short));
	bmap = (unsigned short *)malloc(2*sizeof(short));
	rmap[0] = gmap[0] = bmap[0] = 0;
	rmap[1] = gmap[1] = bmap[1] = 255;
    }

    /* alloc the pixel buffer */
    buffer_size = Image_Size(&hdr);
    if((buffer = malloc(buffer_size)) == NULL)
      Error("Can't malloc data buffer.");

    /* read in the image data */
    count = fread(buffer, sizeof(char), (int)buffer_size, in_file);
    if (count != buffer_size)
        Error("Unable to read pixmap from dump file.");

     /* close the input file */
    fclose(in_file);

/* initialize the input image */
    in_image.width = (int) hdr.pixmap_width;
    in_image.height = (int) hdr.pixmap_height;
    in_image.xoffset = (int) hdr.xoffset;
    in_image.format = (int) hdr.pixmap_format;
    in_image.data = buffer;
    in_image.byte_order = (int) hdr.byte_order;
    in_image.bitmap_unit = (int) hdr.bitmap_unit;
    in_image.bitmap_bit_order = (int) hdr.bitmap_bit_order;
    in_image.bitmap_pad = (int) hdr.bitmap_pad;
    in_image.depth = (int) hdr.pixmap_depth;
    in_image.bytes_per_line = (int) hdr.bytes_per_line;
    in_image.bits_per_pixel = (int) hdr.bits_per_pixel;
    in_image.red_mask = hdr.red_mask;
    in_image.green_mask = hdr.green_mask;
    in_image.blue_mask = hdr.blue_mask;
    in_image.obdata = NULL;

    switch(hdr.pixmap_format) {
        case XYBitmap:
        case XYPixmap:
        case ZPixmap:
	    writeimage(argv[2],&in_image,rmap,gmap,bmap,hdr.visual_class);
	    break;
	default:
	    fprintf(stderr,"strange pixmap format\n");
	    break;
    }
    exit(0);
}

unsigned Image_Size(xi)
XWDFileHeader *xi;
{
    if (xi->pixmap_format == ZPixmap)
	return ((unsigned)xi->bytes_per_line * xi->pixmap_height);
    else
      	return (xi->bytes_per_line * xi->pixmap_depth * xi->pixmap_height);
}

Error(string)
char *string;
{
    fprintf(stderr, "xwud: Error => %s\n", string);
    exit(1);
}

_swapshort (bp, n)
register char *bp;
register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
}

_swaplong (bp, n)
register char *bp;
register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
}

/*
 *	XGetPixel follows
 *
 */

static unsigned char const _reverse_byte[0x100] = {
	0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
	0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
	0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
	0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
	0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
	0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
	0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
	0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
	0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
	0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
	0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
	0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
	0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
	0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
	0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
	0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
	0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
	0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
	0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
	0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
	0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
	0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
	0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
	0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
	0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
	0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
	0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
	0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
	0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
	0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
	0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
	0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};

static void _Reverse_Bytes (bpt, nb)
    register unsigned char *bpt;
    register int nb;
{
    do {
	*bpt = _reverse_byte[*bpt];
	bpt++;
    } while (--nb > 0);
    return;
}

static _normalizeimagebits (bpt, nb, byteorder, unitsize, bitorder)
    unsigned char *bpt;	/* beginning pointer to image bits */
    int nb;		/* number of bytes to normalize */
    int byteorder;	/* swap bytes if byteorder == MSBFirst */
    int unitsize;	/* size of the bitmap_unit or Zpixel */
    int bitorder;	/* swap bits if bitorder == MSBFirst */
{
	if ((byteorder==MSBFirst) && (byteorder!=bitorder)) {
	    register char c;
	    register unsigned char *bp = bpt;
	    register unsigned char *ep = bpt + nb;
	    register unsigned char *sp;
	    switch (unitsize) {

		case 4:
		    do {			/* swap nibble */
			*bp = ((*bp >> 4) & 0xF) | ((*bp << 4) & ~0xF);
			bp++;
		    }
		    while (bp < ep);
		    break;

		case 16:
		    do {			/* swap short */
			c = *bp;
			*bp = *(bp + 1);
			bp++;
			*bp = c;
			bp++;
		    }
		    while (bp < ep);
		    break;

		case 24:
		    do {			/* swap three */
			c = *(bp + 2);
			*(bp + 2) = *bp;
			*bp = c;
			bp += 3;		
		    }
		    while (bp < ep);
		    break;

		case 32:
		    do {			/* swap long */
			sp = bp + 3;
			c = *sp;
			*sp = *bp;
			*bp++ = c;
			sp = bp + 1;
			c = *sp;
			*sp = *bp;
			*bp++ = c;
			bp += 2;
		    }
		    while (bp < ep);
		    break;
	    }
	}
	if (bitorder == MSBFirst) {
	    _Reverse_Bytes (bpt, nb);
	}
}

/*
 * Macros
 * 
 * The ROUNDUP macro rounds up a quantity to the specified boundary.
 *
 * The XYNORMALIZE macro determines whether XY format data requires 
 * normalization and calls a routine to do so if needed. The logic in
 * this module is designed for LSBFirst byte and bit order, so 
 * normalization is done as required to present the data in this order.
 *
 * The ZNORMALIZE macro performs byte and nibble order normalization if 
 * required for Z format data.
 *
 * The XYINDEX macro computes the index to the starting byte (char) boundary
 * for a bitmap_unit containing a pixel with coordinates x and y for image
 * data in XY format.
 * 
 * The ZINDEX macro computes the index to the starting byte (char) boundary 
 * for a pixel with coordinates x and y for image data in ZPixmap format.
 * 
 */

#define ROUNDUP(nbytes, pad) ((((nbytes) + ((pad) - 1)) / (pad)) * (pad))

#define XYNORMALIZE(bp, nbytes, img) \
    if ((img->byte_order == MSBFirst) || (img->bitmap_bit_order == MSBFirst)) \
	_normalizeimagebits((unsigned char *)(bp), (nbytes), img->byte_order, img->bitmap_unit, \
	    img->bitmap_bit_order)

#define ZNORMALIZE(bp, nbytes, img) \
    if (img->byte_order == MSBFirst) \
	_normalizeimagebits((unsigned char *)(bp), (nbytes), MSBFirst, img->bits_per_pixel, \
	LSBFirst)

#define XYINDEX(x, y, img) \
    ((y) * img->bytes_per_line) + \
    (((x) + img->xoffset) / img->bitmap_unit) * (img->bitmap_unit >> 3)

#define ZINDEX(x, y, img) ((y) * img->bytes_per_line) + \
    (((x) * img->bits_per_pixel) >> 3)

/*
 * GetPixel
 * 
 * Returns the specified pixel.  The X and Y coordinates are relative to 
 * the origin (upper left [0,0]) of the image.  The pixel value is returned
 * in normalized format, i.e. the LSB of the long is the LSB of the pixel.
 * The algorithm used is:
 *
 *	copy the source bitmap_unit or Zpixel into temp
 *	normalize temp if needed
 *	extract the pixel bits into return value
 *
 */

static unsigned long const low_bits_table[] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
    0xffffffff
};

static unsigned long _XGetPixel( XImage *ximage, int x, int y )
{
	unsigned long pixel, px;
	register char *src;
	register char *dst;
	register int i, j;
	int bits, nbytes;
	long plane;
     
	if (ximage->depth == 1) {
		src = &ximage->data[XYINDEX(x, y, ximage)];
		dst = (char *)&pixel;
		pixel = 0;
		nbytes = ximage->bitmap_unit >> 3;
		for (i=0; i < nbytes; i++) *dst++ = *src++;
		XYNORMALIZE(&pixel, nbytes, ximage);
          	bits = (x + ximage->xoffset) % ximage->bitmap_unit;
		pixel = ((((char *)&pixel)[bits>>3])>>(bits&7)) & 1;
	} else if (ximage->format == XYPixmap) {
		pixel = 0;
		plane = 0;
		nbytes = ximage->bitmap_unit >> 3;
		for (i=0; i < ximage->depth; i++) {
		    src = &ximage->data[XYINDEX(x, y, ximage)+ plane];
		    dst = (char *)&px;
		    px = 0;
		    for (j=0; j < nbytes; j++) *dst++ = *src++;
		    XYNORMALIZE(&px, nbytes, ximage);
		    bits = (x + ximage->xoffset) % ximage->bitmap_unit;
		    pixel = (pixel << 1) |
			    (((((char *)&px)[bits>>3])>>(bits&7)) & 1);
		    plane = plane + (ximage->bytes_per_line * ximage->height);
		}
	} else if (ximage->format == ZPixmap) {
		src = &ximage->data[ZINDEX(x, y, ximage)];
		dst = (char *)&px;
		px = 0;
		nbytes = ROUNDUP(ximage->bits_per_pixel, 8) >> 3;
		for (i=0; i < nbytes; i++) *dst++ = *src++;		
		ZNORMALIZE(&px, nbytes, ximage);
		pixel = 0;
		for (i=sizeof(unsigned long); --i >= 0; )
		    pixel = (pixel << 8) | ((unsigned char *)&px)[i];
		if (ximage->bits_per_pixel == 4) {
		    if (x & 1)
			pixel >>= 4;
		    else
			pixel &= 0xf;
		}
	} else {
	    fprintf(stderr, "XGetPixel: bad image!!\n");
	    exit(1);
	}
	if (ximage->bits_per_pixel == ximage->depth)
	  return pixel;
	else
	  return (pixel & low_bits_table[ximage->depth]);
}

static unsigned long _XGetPixel8 ( XImage *ximage, int x, int y )
{
	unsigned char pixel;

	pixel = ((unsigned char *)ximage->data)
		    [y * ximage->bytes_per_line + x];
	if (ximage->depth != 8)
	    pixel &= low_bits_table[ximage->depth];
	return pixel;
}

static unsigned long _XGetPixel1 ( XImage *ximage, int x, int y )
{
	unsigned char bit;
	int xoff, yoff;

	xoff = x + ximage->xoffset;
	yoff = y * ximage->bytes_per_line + (xoff >> 3);
	xoff &= 7;
	if (ximage->bitmap_bit_order == MSBFirst)
	    bit = 0x80 >> xoff;
	else
	    bit = 1 << xoff;
	return (ximage->data[yoff] & bit) ? 1 : 0;
}
	
unsigned long MyXGetPixel ( XImage *image, int x, int y )
{
	if ((image->format == ZPixmap) && (image->bits_per_pixel == 8)) {
	    return _XGetPixel8(image,x,y);
	} else if ((image->depth == 1) &&
		   (image->byte_order == image->bitmap_bit_order)) {
	    return _XGetPixel1(image,x,y);
	} else {
	    return _XGetPixel(image,x,y);
	}
}
