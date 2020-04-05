/* graphics.pex.c */

/* Graphics functions for PEX */


/* Vis5D version 5.0 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 1997 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/PEX5/PEXlib.h>
#include <X11/PEX5/PEXHPlib.h>
#include <pexutcmap.h>

#include "globals.h"
#include "graphics.h"
#include "matrix.h"


typedef float Matrix[4][4];


/*** Private Variables: ***/
static float Scale;
static Matrix ModelMat, InvModelMat, ProjMat;
static PEXMatrix IdMat = { {1.0, 0.0, 0.0, 0.0},
			   {0.0, 1.0, 0.0, 0.0},
			   {0.0, 0.0, 1.0, 0.0},
			   {0.0, 0.0, 0.0, 1.0} };
static int Perspective = 0;

static Context current_ctx = NULL;


/******** PEX includes and variables *********/
#define VERTS_PER_CALL 400

static PEXRenderer Renderer;
static PEXRendererAttributes RendererAttrs;
static PEXMatrix InvertYMatrix;
static int InvertY;
static PEXMatrix MCtoDCMatrix;
static PEXMatrix DCtoMCMatrix;



void init_graphics2( void )
{
   HQR_available = 0;
   Perspec_available = 1;
}



void terminate_graphics( void )
{
}



void free_graphics( Display_Context dtx )
{
  /* TODO */
}



void context_init( Context ctx, long win_id, int width, int height )
{
   /* nothing for PEX */
}


int CheckAlphaSupport (display, window)
Display 	*display; 
Window 	window;
{
    int status, j, found;
    int enum_types[1];
    unsigned long *enum_counts;
    PEXEnumTypeDesc *enum_values, *p_enum;
	
    /*
	Determine whether the Renderer can support alpha blending.
    */

    enum_types[0] = PEXHPETTransparencyMethod;

    status = PEXGetEnumTypeInfo (display, window,
			1, enum_types, 
			PEXETCounts|PEXETIndex,
			&enum_counts, &enum_values);
    if (!status) {
	fprintf(stderr, "inquiry of enumerated types failed - exiting\n");
	exit(1);
    }

    p_enum = enum_values;
    found = False;

    for (j=0; j<enum_counts[0]; j++) {
	if (((unsigned short) (p_enum++)->index) == 
	    ((unsigned short) PEXHPTransparencyMethodAlphaBlend)) {
	    found = True;
	}
    }

    PEXFreeEnumInfo (1, enum_counts, enum_values);

    return found;

} /* CheckAlphaSupport */

int make_big_window( char *title, int xpos, int ypos, int width, int height)
{
    PEXUtVisualCriteria         VisualCriteria;
    XVisualInfo                 VisualInfo;
    XStandardColormap           ColormapInfo;
    PEXColorApproxEntry         ColorApproxInfo;
    unsigned int                UnmetCriteria;
    Atom                        StandardPropertyAtom;
    int                         PexUtReturn;
    PEXUtWindowSpecification    WindowSpecification;
    XColor                      Background;
    PEXExtensionInfo            *ExtensionInfo;
    char                        Error[81];
    XWindowAttributes           window_attrs;
    Window wmain;

    unsigned long RenderMask, PipeMask[3];
    PEXPCAttributes PipelineAttrs;


   if (BigWindow){
      printf("BigWindow already exists, so not making it again!!\n");
      return 0;
   }      
    if (PEXInitialize(GfxDpy, &ExtensionInfo, 80, Error) != 0) {
        printf("PEXInitialize failed: %s\n", Error);
        exit(-1);
    }

    WindowSpecification.attr_mask = CWEventMask;
    WindowSpecification.attrs.event_mask =
        ExposureMask | PointerMotionMask | KeyPressMask | ButtonPressMask
        | ButtonReleaseMask | StructureNotifyMask | VisibilityChangeMask;
    WindowSpecification.title = title;
    WindowSpecification.size_hints.x = xpos;
    WindowSpecification.size_hints.y = ypos;
    WindowSpecification.size_hints.width  = width;
    WindowSpecification.size_hints.height = height;
    WindowSpecification.size_hints.max_width = width;
    WindowSpecification.size_hints.max_height = height;
    /* WindowSpecification.size_hints.flags = USSize | USPosition | PMaxSize; */
    WindowSpecification.size_hints.flags = USSize | USPosition;
   if (DisplayMatrix == 11 && BigWindow != NULL){
      WindowSpecification.parent = RootWindow(GfxDpy,GfxScr);
   }
   else {
      WindowSpecification.parent = BigWindow;
   }

    WindowSpecification.parent = RootWindow(GfxDpy,GfxScr);
    WindowSpecification.border_width = 0;
    WindowSpecification.background_color_name = "black";
    WindowSpecification.border_color_name = "black";

    VisualCriteria.hard_criteria_mask = 0;
    VisualCriteria.soft_criteria_mask = 0;

    VisualCriteria.soft_criteria_mask |= PEXUtStandardColormapProperty;
    VisualCriteria.standard_colormap_property = True;
    VisualCriteria.hard_criteria_mask |= PEXUtDoubleBufferingCapability;
    VisualCriteria.double_buffering_capability = True;

    /*
    ** Try to find a visual with PEX double-buffering.
    */
    PexUtReturn = PEXUtCreateWindowAndColormap(
        GfxDpy,
        GfxScr,
        &VisualCriteria,
        &WindowSpecification,
        &wmain,
        &VisualInfo,
        &ColormapInfo,
        &ColorApproxInfo,
        &UnmetCriteria,
        &StandardPropertyAtom,
        &Background);

    /*
    ** If PEXUtCreateWindowAndColormap() was unable to find a visual,
    ** try again for a visual that doesn't support double-buffering.
    */
    if ( PexUtReturn == PEXUtCriteriaFailure ) {
            VisualCriteria.hard_criteria_mask = 0;
            PexUtReturn = PEXUtCreateWindowAndColormap(
                GfxDpy,
                GfxScr,
                &VisualCriteria,
                &WindowSpecification,
                &wmain,
                &VisualInfo,
                &ColormapInfo,
                &ColorApproxInfo,
                &UnmetCriteria,
                &StandardPropertyAtom,
                &Background);
    }

    /*
    ** If PEXUtCreateWindowAndColormap() was still unable to create a window,
    ** report an error and exit.
    */
    if ( ( PexUtReturn != PEXUtSuccess ) &&
         ( PexUtReturn != PEXUtQualifiedSuccess ) ) {

        printf("Error: couldn't open 3-D window\n");
        exit(-1);
    }

    if (XGetWindowAttributes(GfxDpy, wmain, &window_attrs) == 0) {
        printf("Error: couldn't get window attributes\n");
        exit(-1);
    }

    BigWindow = wmain;
    BigWinWidth = window_attrs.width;
    BigWinHeight = window_attrs.height;

    /* set pointer shape */
    XDefineCursor( GfxDpy, wmain, XCreateFontCursor(GfxDpy,XC_top_left_arrow) );

    PipeMask[0] = PipeMask[1] = PipeMask[2] = 0;
    PEXSetPCAttributeMask(PipeMask, PEXPCCharHeight);
    PipelineAttrs.char_height = 9.0;
    PEXSetPCAttributeMask(PipeMask, PEXPCInteriorStyle);
    PipelineAttrs.interior_style = PEXInteriorStyleSolid;
    PEXSetPCAttributeMask(PipeMask, PEXPCSurfaceColor);
    PipelineAttrs.surface_color.type = PEXColorTypeRGB;
    PipelineAttrs.surface_color.value.rgb.red = 1.0;
    PipelineAttrs.surface_color.value.rgb.green = 1.0;
    PipelineAttrs.surface_color.value.rgb.blue = 1.0;
    PEXSetPCAttributeMask(PipeMask, PEXPCReflectionModel);
    PipelineAttrs.reflection_model = PEXReflectionSpecular;
    PEXSetPCAttributeMask(PipeMask, PEXPCReflectionAttr);
    PipelineAttrs.reflection_attr.ambient = 1;
    PipelineAttrs.reflection_attr.diffuse = 1;
    PipelineAttrs.reflection_attr.transmission = 0;
    PipelineAttrs.reflection_attr.specular = 1;
    PipelineAttrs.reflection_attr.specular_conc = 30;
    PipelineAttrs.reflection_attr.specular_color.type = PEXColorTypeRGB;
    PipelineAttrs.reflection_attr.specular_color.value.rgb.red = 1;
    PipelineAttrs.reflection_attr.specular_color.value.rgb.green = 1;
    PipelineAttrs.reflection_attr.specular_color.value.rgb.blue = 1;
    PEXSetPCAttributeMask(PipeMask, PEXPCSurfaceInterp);
    PipelineAttrs.surface_interp = PEXSurfaceInterpColor;
    PEXSetPCAttributeMask(PipeMask, PEXPCHLHSRIdentifier);
    PipelineAttrs.hlhsr_id = PEXHLHSRIDEnable;
    PEXSetPCAttributeMask(PipeMask, PEXPCDepthCueIndex);
    PipelineAttrs.depth_cue_index = 0;
    RendererAttrs.pipeline_context =
        PEXCreatePipelineContext(GfxDpy, PipeMask, &PipelineAttrs);
    RendererAttrs.clear_image = True;
    RendererAttrs.clear_z = True;
    RendererAttrs.background_color.type = PEXColorTypeRGB;
    RendererAttrs.background_color.value.rgb.red = 0.0;
    RendererAttrs.background_color.value.rgb.green = 0.0;
    RendererAttrs.background_color.value.rgb.blue = 0.0;
    RendererAttrs.hlhsr_mode = PEXHLHSRZBuffer;
    RendererAttrs.light_table = PEXCreateLookupTable(GfxDpy, wmain, PEXLUTLight);
    if (RendererAttrs.light_table == 0) {
        printf( "Unable to create PEX light table\n" );
        exit(-1);
    }
    RendererAttrs.view_table = PEXCreateLookupTable(GfxDpy, wmain, PEXLUTView);
    if (RendererAttrs.view_table == 0) {
        printf( "Unable to create PEX view table\n" );
        exit(-1);
    }
    RendererAttrs.depth_cue_table = PEXCreateLookupTable(GfxDpy, wmain,
        PEXLUTDepthCue);
    if (RendererAttrs.depth_cue_table == 0) {
        printf( "Unable to create PEX depth cue table\n" );
        exit(-1);
    }
    RenderMask = PEXRAHLHSRMode | PEXRABackgroundColor | PEXRAClearImage
        | PEXRAClearZ | PEXRALightTable | PEXRAViewTable | PEXRADepthCueTable
        | PEXRAPipelineContext;

    {
        PEXLightEntry lights[3];
        PEXTableIndex lights_on[1];

        /* Define the ambient light. */
        lights[0].type = PEXLightAmbient;
        lights[0].color.type = PEXColorTypeRGB;
        lights[0].color.value.rgb.red = 0.2;
        lights[0].color.value.rgb.green = 0.2;
        lights[0].color.value.rgb.blue = 0.2;

        /* Define a directional light. */
        lights[1].type = PEXLightWCVector;
        lights[1].direction.x = 0;
        lights[1].direction.y = 0;
        lights[1].direction.z = 10;
        lights[1].color.type = PEXColorTypeRGB;
        lights[1].color.value.rgb.red = 0.8;
        lights[1].color.value.rgb.green = 0.8;
        lights[1].color.value.rgb.blue = 0.8;

        /* Define another directional light. */
        lights[2] = lights[1];
        lights[2].direction.z = -10;

        /* Add the lights to the light table. */
/*
        PEXSetTableEntries( GfxDpy, RendererAttrs.light_table, 1, 1, PEXLUTLight,
            lights );
        PEXSetTableEntries( GfxDpy, RendererAttrs.light_table, 2, 1, PEXLUTLight,
            lights );
        PEXSetTableEntries( GfxDpy, RendererAttrs.light_table, 3, 1, PEXLUTLight,
            lights );
*/


        PEXSetTableEntries( GfxDpy, RendererAttrs.light_table,
                            1, 3, PEXLUTLight, lights );


        /* enable ambient light */
/*        lights_on[0] = 1;*/
/*
        PEXSetLightSourceState(GfxDpy, Renderer, PEXOCRender, 1, lights_on,
                               0, (PEXTableIndex*)NULL );
*/
    }

    {
        PEXDepthCueEntry DepthCues[2];

        /* Define the no depth cue entry. */
        DepthCues[0].mode = False;
        DepthCues[0].front_plane = 1.0;
        DepthCues[0].front_scaling = 1.0;
        DepthCues[0].back_plane = 0.0;
        DepthCues[0].back_scaling = 0.2;
        DepthCues[0].color.type = PEXColorTypeRGB;
        DepthCues[0].color.value.rgb.red = 0.0;
        DepthCues[0].color.value.rgb.green = 0.0;
        DepthCues[0].color.value.rgb.blue = 0.0;

        /* Define the depth cue entry. */
        DepthCues[1].mode = True;
        DepthCues[1].front_plane = 1.0;
        DepthCues[1].front_scaling = 1.0;
        DepthCues[1].back_plane = 0.0;
        DepthCues[1].back_scaling = 0.2;
        DepthCues[1].color.type = PEXColorTypeRGB;
        DepthCues[1].color.value.rgb.red = 0.0;
        DepthCues[1].color.value.rgb.green = 0.0;
        DepthCues[1].color.value.rgb.blue = 0.0;

        /* Add the depth cues to the table. */
        PEXSetTableEntries(GfxDpy, RendererAttrs.depth_cue_table, 0, 2,
            PEXLUTDepthCue, DepthCues);
    }

    Renderer = PEXCreateRenderer(GfxDpy, wmain, RenderMask, &RendererAttrs);
    if (Renderer == 0) {
        printf( "Unable to create PEX renderer\n" );
        exit(-1);
    }

    XSync(GfxDpy, False);

    resize_3d_window(width, height);

    return wmain;
}



/*
 * Make a 3-D graphics window.
 * Input:  
 *         ctx - the Vis5D context
 *         title - window title string
 *         xpos,ypos - position in pixel from upper-left corner
 *         width, height - width and height of window in pixels
 * Return:  X window pointer or NULL.
 */
int make_3d_window( Display_Context dtx, char *title, int xpos, int ypos,
                    int width, int height )
{
    PEXUtVisualCriteria		VisualCriteria;
    XVisualInfo			VisualInfo;
    XStandardColormap		ColormapInfo;
    PEXColorApproxEntry		ColorApproxInfo;
    unsigned int		UnmetCriteria;
    Atom			StandardPropertyAtom;
    int				PexUtReturn;
    PEXUtWindowSpecification	WindowSpecification;
    XColor			Background;
    PEXExtensionInfo		*ExtensionInfo;
    char			Error[81];
    XWindowAttributes		window_attrs;
    Window wmain;

    unsigned long RenderMask, PipeMask[3];
    PEXPCAttributes PipelineAttrs;

    current_ctx = ctx;

    if (PEXInitialize(GfxDpy, &ExtensionInfo, 80, Error) != 0) {
	printf("PEXInitialize failed: %s\n", Error);
	exit(-1);
    }

    WindowSpecification.attr_mask = CWEventMask;
    WindowSpecification.attrs.event_mask =
	ExposureMask | PointerMotionMask | KeyPressMask | ButtonPressMask
	| ButtonReleaseMask | StructureNotifyMask | VisibilityChangeMask;
    WindowSpecification.title = title;
    WindowSpecification.size_hints.x = xpos;
    WindowSpecification.size_hints.y = ypos;
    WindowSpecification.size_hints.width  = width;
    WindowSpecification.size_hints.height = height;
    WindowSpecification.size_hints.max_width = width;
    WindowSpecification.size_hints.max_height = height;
    /* WindowSpecification.size_hints.flags = USSize | USPosition | PMaxSize; */
    WindowSpecification.size_hints.flags = USSize | USPosition;
   if (DisplayMatrix == 11 && BigWindow != NULL){
      WindowSpecification.parent = RootWindow(GfxDpy,GfxScr);
   }
   else {
      WindowSpecification.parent = BigWindow;
   }

    WindowSpecification.parent = RootWindow(GfxDpy,GfxScr);
    WindowSpecification.border_width = 0;
    WindowSpecification.background_color_name = "black";
    WindowSpecification.border_color_name = "black";

    VisualCriteria.hard_criteria_mask = 0;
    VisualCriteria.soft_criteria_mask = 0;

    VisualCriteria.soft_criteria_mask |= PEXUtStandardColormapProperty;
    VisualCriteria.standard_colormap_property = True;
    VisualCriteria.hard_criteria_mask |= PEXUtDoubleBufferingCapability;
    VisualCriteria.double_buffering_capability = True;

    /*
    ** Try to find a visual with PEX double-buffering.
    */
    PexUtReturn = PEXUtCreateWindowAndColormap(
	GfxDpy,
	GfxScr,
	&VisualCriteria,
	&WindowSpecification,
	&wmain,
	&VisualInfo,
	&ColormapInfo,
	&ColorApproxInfo,
	&UnmetCriteria,
	&StandardPropertyAtom,
	&Background);

    /*
    ** If PEXUtCreateWindowAndColormap() was unable to find a visual,
    ** try again for a visual that doesn't support double-buffering.
    */
    if ( PexUtReturn == PEXUtCriteriaFailure ) {
	    VisualCriteria.hard_criteria_mask = 0;
	    PexUtReturn = PEXUtCreateWindowAndColormap(
		GfxDpy,
		GfxScr,
		&VisualCriteria,
		&WindowSpecification,
		&wmain,
		&VisualInfo,
		&ColormapInfo,
		&ColorApproxInfo,
		&UnmetCriteria,
		&StandardPropertyAtom,
		&Background);
    }

    /*
    ** If PEXUtCreateWindowAndColormap() was still unable to create a window,
    ** report an error and exit.
    */
    if ( ( PexUtReturn != PEXUtSuccess ) &&
	 ( PexUtReturn != PEXUtQualifiedSuccess ) ) {

	printf("Error: couldn't open 3-D window\n");
	exit(-1);
    }

    if (XGetWindowAttributes(GfxDpy, wmain, &window_attrs) == 0) {
	printf("Error: couldn't get window attributes\n");
	exit(-1);
    }

    ctx->GfxWindow = wmain;
    ctx->WinWidth = window_attrs.width;
    ctx->WinHeight = window_attrs.height;
    ctx->FontHeight = (int) 9.0;   /*TODO FIX*/
    ctx->FontDescent = (int) 0.1;

    /* set pointer shape */
    XDefineCursor( GfxDpy, wmain, XCreateFontCursor(GfxDpy,XC_top_left_arrow) );

    PipeMask[0] = PipeMask[1] = PipeMask[2] = 0;
    PEXSetPCAttributeMask(PipeMask, PEXPCCharHeight);
    PipelineAttrs.char_height = 9.0;
    PEXSetPCAttributeMask(PipeMask, PEXPCInteriorStyle);
    PipelineAttrs.interior_style = PEXInteriorStyleSolid;
    PEXSetPCAttributeMask(PipeMask, PEXPCSurfaceColor);
    PipelineAttrs.surface_color.type = PEXColorTypeRGB;
    PipelineAttrs.surface_color.value.rgb.red = 1.0;
    PipelineAttrs.surface_color.value.rgb.green = 1.0;
    PipelineAttrs.surface_color.value.rgb.blue = 1.0;
    PEXSetPCAttributeMask(PipeMask, PEXPCReflectionModel);
    PipelineAttrs.reflection_model = PEXReflectionSpecular;
    PEXSetPCAttributeMask(PipeMask, PEXPCReflectionAttr);
    PipelineAttrs.reflection_attr.ambient = 1;
    PipelineAttrs.reflection_attr.diffuse = 1;
    PipelineAttrs.reflection_attr.transmission = 0;
    PipelineAttrs.reflection_attr.specular = 1;
    PipelineAttrs.reflection_attr.specular_conc = 30;
    PipelineAttrs.reflection_attr.specular_color.type = PEXColorTypeRGB;
    PipelineAttrs.reflection_attr.specular_color.value.rgb.red = 1;
    PipelineAttrs.reflection_attr.specular_color.value.rgb.green = 1;
    PipelineAttrs.reflection_attr.specular_color.value.rgb.blue = 1;
    PEXSetPCAttributeMask(PipeMask, PEXPCSurfaceInterp);
    PipelineAttrs.surface_interp = PEXSurfaceInterpColor;
    PEXSetPCAttributeMask(PipeMask, PEXPCHLHSRIdentifier);
    PipelineAttrs.hlhsr_id = PEXHLHSRIDEnable;
    PEXSetPCAttributeMask(PipeMask, PEXPCDepthCueIndex);
    PipelineAttrs.depth_cue_index = 0;
    RendererAttrs.pipeline_context =
	PEXCreatePipelineContext(GfxDpy, PipeMask, &PipelineAttrs);
    RendererAttrs.clear_image = True;
    RendererAttrs.clear_z = True;
    RendererAttrs.background_color.type = PEXColorTypeRGB;
    RendererAttrs.background_color.value.rgb.red = 0.0;
    RendererAttrs.background_color.value.rgb.green = 0.0;
    RendererAttrs.background_color.value.rgb.blue = 0.0;
    RendererAttrs.hlhsr_mode = PEXHLHSRZBuffer;
    RendererAttrs.light_table = PEXCreateLookupTable(GfxDpy, wmain, PEXLUTLight);
    if (RendererAttrs.light_table == 0) {
	printf( "Unable to create PEX light table\n" );
	exit(-1);
    }
    RendererAttrs.view_table = PEXCreateLookupTable(GfxDpy, wmain, PEXLUTView);
    if (RendererAttrs.view_table == 0) {
	printf( "Unable to create PEX view table\n" );
	exit(-1);
    }
    RendererAttrs.depth_cue_table = PEXCreateLookupTable(GfxDpy, wmain,
	PEXLUTDepthCue);
    if (RendererAttrs.depth_cue_table == 0) {
	printf( "Unable to create PEX depth cue table\n" );
	exit(-1);
    }
    RenderMask = PEXRAHLHSRMode | PEXRABackgroundColor | PEXRAClearImage
	| PEXRAClearZ | PEXRALightTable | PEXRAViewTable | PEXRADepthCueTable
	| PEXRAPipelineContext;

    {
	PEXLightEntry lights[3];
        PEXTableIndex lights_on[1];

	/* Define the ambient light. */
	lights[0].type = PEXLightAmbient;
	lights[0].color.type = PEXColorTypeRGB;
	lights[0].color.value.rgb.red = 0.2;
	lights[0].color.value.rgb.green = 0.2;
	lights[0].color.value.rgb.blue = 0.2;

	/* Define a directional light. */
	lights[1].type = PEXLightWCVector;
	lights[1].direction.x = 0;
	lights[1].direction.y = 0;
	lights[1].direction.z = 10;
	lights[1].color.type = PEXColorTypeRGB;
	lights[1].color.value.rgb.red = 0.8;
	lights[1].color.value.rgb.green = 0.8;
	lights[1].color.value.rgb.blue = 0.8;

	/* Define another directional light. */
	lights[2] = lights[1];
	lights[2].direction.z = -10;

	/* Add the lights to the light table. */
/*
	PEXSetTableEntries( GfxDpy, RendererAttrs.light_table, 1, 1, PEXLUTLight,
	    lights );
	PEXSetTableEntries( GfxDpy, RendererAttrs.light_table, 2, 1, PEXLUTLight,
	    lights );
	PEXSetTableEntries( GfxDpy, RendererAttrs.light_table, 3, 1, PEXLUTLight,
	    lights );
*/


	PEXSetTableEntries( GfxDpy, RendererAttrs.light_table,
			    1, 3, PEXLUTLight, lights );


        /* enable ambient light */
/*        lights_on[0] = 1;*/
/*
        PEXSetLightSourceState(GfxDpy, Renderer, PEXOCRender, 1, lights_on,
			       0, (PEXTableIndex*)NULL );
*/
    }

    {
	PEXDepthCueEntry DepthCues[2];

	/* Define the no depth cue entry. */
	DepthCues[0].mode = False;
	DepthCues[0].front_plane = 1.0;
	DepthCues[0].front_scaling = 1.0;
	DepthCues[0].back_plane = 0.0;
	DepthCues[0].back_scaling = 0.2;
	DepthCues[0].color.type = PEXColorTypeRGB;
	DepthCues[0].color.value.rgb.red = 0.0;
	DepthCues[0].color.value.rgb.green = 0.0;
	DepthCues[0].color.value.rgb.blue = 0.0;

	/* Define the depth cue entry. */
	DepthCues[1].mode = True;
	DepthCues[1].front_plane = 1.0;
	DepthCues[1].front_scaling = 1.0;
	DepthCues[1].back_plane = 0.0;
	DepthCues[1].back_scaling = 0.2;
	DepthCues[1].color.type = PEXColorTypeRGB;
	DepthCues[1].color.value.rgb.red = 0.0;
	DepthCues[1].color.value.rgb.green = 0.0;
	DepthCues[1].color.value.rgb.blue = 0.0;

	/* Add the depth cues to the table. */
	PEXSetTableEntries(GfxDpy, RendererAttrs.depth_cue_table, 0, 2,
	    PEXLUTDepthCue, DepthCues);
    }

    Renderer = PEXCreateRenderer(GfxDpy, wmain, RenderMask, &RendererAttrs);
    if (Renderer == 0) {
	printf( "Unable to create PEX renderer\n" );
	exit(-1);
    }

    XSync(GfxDpy, False);
    if (PEXUtDBConfig(GfxDpy, wmain, True, &Background, False, &ctx->db_win)) {
	ctx->use_db = False;
	ctx->db_win = wmain;
    } else {
	ctx->use_db = True;
    }
    XSync(GfxDpy, False);

    ctx->has_blending = CheckAlphaSupport(GfxDpy, ctx->db_win);

    resize_3d_window(width, height);

    return wmain;
}



void set_current_window( Context ctx )
{
   current_ctx = ctx;
}


int set_3d_font( Context ctx, char *name, int size )
{
  /*TODO */
}



void set_pointer( int p )
{
   if (p) {
      /* make busy cursor */
      XDefineCursor( GfxDpy, current_ctx->GfxWindow,
		     XCreateFontCursor(GfxDpy,XC_watch) );
   }
   else {
      XDefineCursor( GfxDpy, current_ctx->GfxWindow,
		     XCreateFontCursor(GfxDpy,XC_top_left_arrow) );
   }
}



int save_formats( void )
{
   return VIS5D_XWD;
}



int save_3d_window( char *filename, int format )
{
   FILE *f;
   set_pointer(1);
   /* Make an X window dump file (.xwd) */
   f = fopen(filename,"w");
   if (f) {
      Window_Dump( GfxDpy, GfxScr, current_ctx->GfxWindow, f );
      fclose(f);
      set_pointer(0);
      return 1;
   }
   else {
      printf("Error unable to open %s for writing\n", filename);
      set_pointer(0);
      return 0;
   }
}




int print_3d_window( void )
{
   static char xwd_file[] = "/usr/tmp/VIS-5D_image.xwd";
   char cmd[1000];
   FILE *f;

   set_pointer(1);

   /* Make an X window dump file (.xwd) */
   f = fopen(xwd_file,"w");
   if (f) {
      printf("Writing X window dump: %s\n", xwd_file);
      Window_Dump( GfxDpy, GfxScr, current_ctx->GfxWindow, f );
      fclose(f);

      /* Use xpr to print the window dump */
      if (!installed("xpr")) return 0;
      sprintf(cmd,"xpr -device ps %s | lpr\n", xwd_file );
      printf("Executing: %s\n", cmd );
      system(cmd);

      /* delete xwd file */
      unlink( xwd_file );

      printf("Done.\n");
      set_pointer(0);
      return 1;
   }
   else {
      printf("Error unable to open %s for writing\n", xwd_file);
      set_pointer(0);
      return 0;
   }
}



void clear_color( unsigned int bgcolor )
{
   unsigned long mask = PEXRABackgroundColor;
   float r, g, b;

   r = UNPACK_RED(bgcolor)/255.0;
   g = UNPACK_GREEN(bgcolor)/255.0;
   b = UNPACK_BLUE(bgcolor)/255.0;

   RendererAttrs.background_color.type = PEXColorTypeRGB;
   RendererAttrs.background_color.value.rgb.red   = r;
   RendererAttrs.background_color.value.rgb.green = g;
   RendererAttrs.background_color.value.rgb.blue  = b;

   PEXChangeRenderer( GfxDpy, Renderer, mask, &RendererAttrs );
}



void clear_3d_window( void )
{
   /* Clear is implicit in BeginRendering in set_3d. */
}


/*
 * Called when window size changes.
 */
void resize_3d_window( int width, int height )
{
/*   printf("resize %d %d\n", width, height );*/
    current_ctx->WinWidth = width;
    current_ctx->WinHeight = height;

    PEXUtDBResize( GfxDpy, current_ctx->GfxWindow );
    {
	float ratio, delta;

	if( width > height)
	{
	    ratio = (float)width / (float)height;
	    delta = (1.0 / ratio) * 0.5;

	    RendererAttrs.npc_subvolume.min.x = 0;
	    RendererAttrs.npc_subvolume.min.y = 0.5 - delta;
	    RendererAttrs.npc_subvolume.min.z = 0;
	    RendererAttrs.npc_subvolume.max.x = 1.0;
	    RendererAttrs.npc_subvolume.max.y = 0.5 + delta;
	    RendererAttrs.npc_subvolume.max.z = 1.0;
	}
	else
	{
	    ratio = (float)height / (float)width;
	    delta = (1.0 / ratio) * 0.5;

	    RendererAttrs.npc_subvolume.min.x = 0.5 - delta;
	    RendererAttrs.npc_subvolume.min.y = 0;
	    RendererAttrs.npc_subvolume.min.z = 0;
	    RendererAttrs.npc_subvolume.max.x = 0.5 + delta;
	    RendererAttrs.npc_subvolume.max.y = 1.0;
	    RendererAttrs.npc_subvolume.max.z = 1.0;
	}
	PEXChangeRenderer(GfxDpy, Renderer, PEXRANPCSubVolume, &RendererAttrs);
    }
   {
	PEXViewEntry	views[2];
	PEXCoord2D	view_window[2];
	double		view_plane,
			front_plane,
			back_plane;
	PEXCoord	prp;
	PEXNPCSubVolume	viewport;
	static PEXCoord	view_ref_pt = {0.0, 0.0, 0.0};
	static PEXVector view_plane_normal = {0.0, 0.0, 1.0};
	static PEXVector view_up_vec = {0,1,0};

	PEXViewOrientationMatrix( &view_ref_pt, &view_plane_normal,
	    &view_up_vec, views[0].orientation );

	PEXViewOrientationMatrix( &view_ref_pt, &view_plane_normal,
	    &view_up_vec, views[1].orientation );

	/*
	** The view mapping parameters.
	*/
	view_window[0].x = 0;
	view_window[0].y = 0;
	view_window[1].x = width-1;
	view_window[1].y = height-1;
	prp.x = view_window[1].x / 2.0;
	prp.y = view_window[1].y / 2.0;
	prp.z = 4.0;
	front_plane = 1;
	view_plane = 0;
	back_plane = -1;
	viewport.min.x = RendererAttrs.npc_subvolume.min.x;
	viewport.max.x = RendererAttrs.npc_subvolume.max.x;
	viewport.min.y = RendererAttrs.npc_subvolume.min.y;
	viewport.max.y = RendererAttrs.npc_subvolume.max.y;
	viewport.min.z = 0;
	viewport.max.z = 1;

	PEXViewMappingMatrix(view_window, &viewport, False, &prp, view_plane,
	    back_plane, front_plane, views[0].mapping);
	views[0].clip_flags = PEXClippingAll;
	views[0].clip_limits = viewport;

	view_window[0].x = -1;
	view_window[0].y = -1;
	view_window[1].x = 1;
	view_window[1].y = 1;
	front_plane = 1;
	view_plane = 0;
	back_plane = -1;
	prp.x = 0.0;
	prp.y = 0.0;
	prp.z = 5.0;

	PEXViewMappingMatrix(view_window, &viewport, False, &prp, view_plane,
	    back_plane, front_plane, views[1].mapping);
	views[1].clip_flags = PEXClippingAll;
	views[1].clip_limits = viewport;

	PEXSetTableEntries(GfxDpy, RendererAttrs.view_table, 0, 2, PEXLUTView,
	    (PEXPointer) views);
    }
}



void swap_3d_window( void )
{
   PEXEndRendering(GfxDpy, Renderer, True);
   if (current_ctx->use_db) {
     current_ctx->db_win = PEXUtDBSwap( GfxDpy, current_ctx->GfxWindow, False);
   }
   XSync (GfxDpy, False);
}




static void decomposition(xform3, pivot)
PEXMatrix xform3;
int pivot[4];
/* Performs the Lu decomposition for Gaussian elimination with partial
 * pivoting.  On entry:
 *   xform3 - array to be decomposed
 *   On exit:
 *   xform3 - contains the LU decomposition
 *   pivot - contains the pivot history...the original position numbers
 *	of the pivoted rows.
 */
{
    register int i, j, step;
    register float temp, mfact;            /*temp & row multiplication factor*/
    register float abs_istep, abs_maxstep; /*used to get absolute values*/
    register int itemp, max;               /*temp & maximum coefficient*/

    for (i = 0; i <= 3; i++)
	pivot[i] = i;
    for (step = 0; step <= 3; step++) {
	max = step;
	for (i = step + 1; i <= 3; i++) {
	    abs_istep = xform3[i][step];
	    if (abs_istep < 0.0)
		abs_istep = -abs_istep;
	    abs_maxstep = xform3[max][step];
	    if (abs_maxstep < 0.0)
		abs_maxstep = -abs_maxstep;
	    if (abs_istep > abs_maxstep)
		max = i;
	}
	if (xform3[max][step] == 0.0) {
	    printf("Couldn't invert a transform.\n");
	    exit(-1);
	}
	if (max != step) {
	    for (j = 0; j <= 3; j++) {
		temp = xform3[step][j];
		xform3[step][j] = xform3[max][j];
		xform3[max][j] = temp;
	    }
	    itemp = pivot[step];
	    pivot[step] = pivot[max];
	    pivot[max] = itemp;
	}
	for (i = step + 1; i <= 3; i++) {
	    mfact = -xform3[i][step] / xform3[step][step];
	    xform3[i][step] = -mfact;
	    for (j = step + 1; j <= 3; j++)
		xform3[i][j] = mfact * xform3[step][j] + xform3[i][j];
	}
    }
}


static void solve(xform3, col, result, pivot)
PEXMatrix xform3;
float col[4], result[4];
int pivot[4];
{
    register int solv, i;
    float sum;              /*used for computing a sum while backsolving*/
    float y[4];             /*used for computing Ly=Pb*/

    /*Solve for y in Ly=Pb*/
    for (solv = 0; solv <= 3; solv++) {
	sum = 0.0;
	for (i = 0; i <= solv - 1; i++)
	    sum = sum + y[i] * xform3[solv][i];
	y[solv] = col[pivot[solv]] - sum;
    }

    /*Solve for x in Ux=y*/
    for (solv = 3; solv >= 0; solv--) {
	sum = 0.0;
	for (i = solv + 1; i <= 3; i++)
	    sum += result[i] * xform3[solv][i];
	result[solv] = (y[solv] - sum) / xform3[solv][solv];
    }
}


static void compute_inverse(xform1, xform2)
PEXMatrix xform1, xform2;
{
    int pivot[4];       /*pivot history - used to remember original*/
			/*row numbers of the pivoted rows*/
    float idcol[4];     /*columns of the identity matrix*/
    float result[4];    /*inverse is solved one column at a time*/
    register int i, j;
    PEXMatrix txform;    /*temp copy of xform1*/

    memcpy(txform, xform1, sizeof(PEXMatrix));
    decomposition(txform, pivot);
    /*Solve system with columns of identity matrix as solutions*/
    for (i = 0; i <= 3; i++) {
	for (j = 0; j <= 3; j++)
	    idcol[j] = 0.0;
	idcol[i] = 1.0;
	solve(txform, idcol, result, pivot);
	/* Resultant columns are the columns of inverse matrix*/
	for (j = 0; j <= 3; j++)
	    xform2[j][i] = result[j];
    }
}



/*
 * Setup for 3-D rendering
 */
void set_3d( int perspective, float frontclip, float zoom, float *modelmat )
{
    PEXViewEntry	view;
    PEXCoord2D		view_window[2];
    double		view_plane,
			front_plane,
			back_plane;
    PEXCoord		prp;
    PEXNPCSubVolume	viewport;
    static PEXCoord	view_ref_pt = {0.0, 0.0, 0.0};
    static PEXVector	view_plane_normal = {0.0, 0.0, 1.0};
    static PEXVector	view_up_vec = {0,1,0};
    PEXMatrix		global;
    register int	i, j;
    PEXTableIndex	lights_on[3];
    PEXVector 		ShiftVector,
			ScaleVector;
    PEXMatrix 		TranslateMatrix,
			ScaleMatrix,
			InvertZMatrix,
			NPCtoXCMatrix;
    PEXDeviceCoord	DCViewPort[2];
    float FrontClip;


    /* This also clears the window */
    PEXBeginRendering(GfxDpy, current_ctx->db_win, Renderer);

    InvertY = False;

    Perspective = perspective;
    Scale = zoom;
    FrontClip = 1.0 - 2.0*frontclip;  /*TODO: verify*/
    if (FrontClip < -0.9) {
       /* to prevent front plane == back plane */
       FrontClip = -0.9;
    }
    /*
    ** The view mapping parameters.
    */
    prp.x = 0.0;
    prp.y = 0.0;
    prp.z = 5.0;
    front_plane = zoom * 1.75 * FrontClip;
    /* prp must be outside front clip plane */
    if (front_plane > 4.9) front_plane = 4.9;
    view_plane = 0.0;
    back_plane = -zoom * 1.75 /*  * FrontClip */;
    viewport.min.x = 0;
    viewport.max.x = 1;
    viewport.min.y = 0;
    viewport.max.y = 1;
    viewport.min.z = 0;
    viewport.max.z = 1;

    view_window[0].x = -1.5;
    view_window[0].y = -1.5;
    view_window[1].x = 1.5;
    view_window[1].y = 1.5;

    PEXViewOrientationMatrix(&view_ref_pt, &view_plane_normal,
	&view_up_vec, view.orientation);

    PEXViewMappingMatrix(view_window, &viewport, Perspective, &prp, view_plane,
	back_plane, front_plane, view.mapping);
    view.clip_flags = PEXClippingAll;
    view.clip_limits = viewport;

    PEXSetTableEntries(GfxDpy, RendererAttrs.view_table, 1, 1, PEXLUTView,
	(PEXPointer) &view);

    PEXSetViewIndex(GfxDpy, Renderer, PEXOCRender, 1);
    RendererAttrs.hlhsr_mode = PEXHLHSRZBuffer;
    PEXChangeRenderer(GfxDpy, Renderer, PEXRAHLHSRMode, &RendererAttrs);

    if ( current_ctx->has_blending ) {
	unsigned long mask[2];
	PEXHPRendererAttributes hp_attrs;


	mask[0] = 0;
	mask[1] = 0;
	PEXHPSetRendererAttributeMask(mask, PEXHPRATransparencyMethod);
	if (current_ctx->AlphaBlend) {
	    hp_attrs.transparency_method =
		(unsigned long) PEXHPTransparencyMethodAlphaBlend;
	} else {
	    hp_attrs.transparency_method =
		(unsigned long) PEXHPTransparencyMethodScreenDoor;
	}
	PEXHPChangeRenderer( GfxDpy, Renderer, mask, &hp_attrs);
	PEXHPSetAlphaBlendFunction( GfxDpy, Renderer, PEXOCRender,
	    PEXHPAlphaBlendFunctionSimpleAlpha);

    }


    /* Turn the lights on. */

    lights_on[0] = 1;
    lights_on[1] = 2;
    lights_on[2] = 3;
    PEXSetLightSourceState(GfxDpy, Renderer, PEXOCRender, 3, lights_on,
	0, (PEXTableIndex*)NULL );


    /* Transpose matrix. */
    for (i=0; i<4; i++) {
	for (j=0; j<4; j++) {
	    global[i][j] = modelmat[j*4+i];
	}
    }

    /* Change the model matrix so a z range of 0 to 1 becomes 1 to 0 */
    ScaleVector.x = ScaleVector.y = ScaleVector.z = Scale;
    PEXScale(&ScaleVector, ScaleMatrix);
    PEXMatrixMult(ScaleMatrix, global, global);
    PEXSetGlobalTransform(GfxDpy, Renderer, PEXOCRender, global);

    /* Create inverse matrix to use in project and unproject. */
    DCViewPort[0].x = DCViewPort[0].y = DCViewPort[0].z = 0.0;
    DCViewPort[1].x = current_ctx->WinWidth;
    DCViewPort[1].y = current_ctx->WinHeight;
    DCViewPort[1].z = 1.0;
    PEXNPCToXCTransform(&viewport, DCViewPort, current_ctx->WinHeight, NPCtoXCMatrix);
    PEXMatrixMult(view.orientation, global, MCtoDCMatrix);
    PEXMatrixMult(view.mapping, MCtoDCMatrix, MCtoDCMatrix);
    PEXMatrixMult(NPCtoXCMatrix, MCtoDCMatrix, MCtoDCMatrix);
    compute_inverse(MCtoDCMatrix, DCtoMCMatrix);
}



/*** set_2d ***********************************************************
   This is to be called prior to any 2-D rendering calls.
**********************************************************************/
void set_2d( void )
{
    PEXVector ShiftVector, ScaleVector;
    PEXMatrix TranslateMatrix, ScaleMatrix;

/* Change the model matrix so a y range of 0 to Height becomes Height to 0 */
    InvertY = True;
    ScaleVector.x = 1.0; ScaleVector.y = -1.0; ScaleVector.z = 1.0;
    PEXScale(&ScaleVector, ScaleMatrix);
    ShiftVector.x = 0.0; ShiftVector.y = current_ctx->WinHeight; ShiftVector.z = 0.0;
    PEXTranslate(&ShiftVector, TranslateMatrix);
    PEXMatrixMult(TranslateMatrix, ScaleMatrix, InvertYMatrix);
    PEXSetGlobalTransform(GfxDpy, Renderer, PEXOCRender, InvertYMatrix);

    PEXSetViewIndex(GfxDpy, Renderer, PEXOCRender, 0);
    RendererAttrs.hlhsr_mode = PEXHLHSROff;
    PEXChangeRenderer(GfxDpy, Renderer, PEXRAHLHSRMode, &RendererAttrs);
}



/*** project **********************************************************
   Use current transformation and viewing information to project a
   point p from 3-D graphics coordinates to 2-D window coordinates
   in [0,WinWidth-1]x[0,WinHeight-1].
   Input:  p - 3-D point in graphics space
   Output:  x, y - 2-D point in window pixel coordinates.
**********************************************************************/
void project( float p[3], float *x, float *y )
{
    PEXCoord DC[1], MC[1];

    MC[0].x = p[0];
    MC[0].y = p[1];
    MC[0].z = p[2];
    PEXTransformPoints(MCtoDCMatrix, 1, MC, DC);
    *x = DC[0].x;
    *y = DC[0].y;
}



/*** unproject ********************************************************
   Given a 2-D window coordinate in [0,WinWidth-1]x[0,WinHeight-1],
   return the parametric equation of a line in 3-D such that the
   projection of the line from 3-D to 2-D is a point at the window
   coordinate.
   Input:  x, y - window coordinate.
   Output:  p, d - parametric equation of line:  l = p + t*d
                   NOTE, d will have unit length
**********************************************************************/
void unproject( float x, float y, float p[3], float d[3] )
{
    PEXCoord DC[2], MC[2];
    float scale;

    DC[0].x = x;
    DC[0].y = y;
    DC[0].z = 1.0;
    DC[1].x = x;
    DC[1].y = y;
    DC[1].z = 0.0;
    PEXTransformPoints(DCtoMCMatrix, 2, DC, MC);
    p[0] = MC[0].x;
    p[1] = MC[0].y;
    p[2] = MC[0].z;
    d[0] = MC[1].x - MC[0].x;
    d[1] = MC[1].y - MC[0].y;
    d[2] = MC[1].z - MC[0].z;
    scale = 1.0 / sqrt(d[0]*d[0] + d[1]*d[1] + d[2]*d[2]);
    d[0] *= scale;
    d[1] *= scale;
    d[2] *= scale;
}



static void enable_lighting( int onoff )
{
   PEXTableIndex lights[3];

   lights[0] = 1;
   lights[1] = 2;
   lights[2] = 3;

   if (onoff) {
      PEXSetReflectionModel( GfxDpy, Renderer, PEXOCRender,
			     PEXReflectionDiffuse /*Specular*/ );
   }
   else {
      PEXSetReflectionModel( GfxDpy, Renderer,
			     PEXOCRender, PEXReflectionNone );
   }
}




void transparency_mode( Context ctx, int mode )
{
   if (mode==1) {
      ctx->AlphaBlend = 1;
   }
   else {
      ctx->AlphaBlend = 0;
   }
}



void set_color( unsigned int c )
{
    PEXColor color;
    PEXReflectionAttributes reflection;
    float alpha;

    color.rgb.red   = UNPACK_RED(c) * 1.0 / 255.0;
    color.rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
    color.rgb.blue  = UNPACK_BLUE(c) * 1.0 / 255.0;
    PEXSetLineColor(GfxDpy, Renderer, PEXOCRender, PEXColorTypeRGB, &color);
    PEXSetSurfaceColor(GfxDpy, Renderer, PEXOCRender, PEXColorTypeRGB, &color);
    PEXSetTextColor(GfxDpy, Renderer, PEXOCRender, PEXColorTypeRGB, &color);

    alpha = UNPACK_ALPHA(c) * 1.0 / 255.0;
    reflection.ambient = 1;
    reflection.diffuse = 1;
    reflection.transmission = 1.0 - alpha;
    reflection.specular = 1;
    reflection.specular_conc = 30;
    reflection.specular_color.type = PEXColorTypeRGB;
    reflection.specular_color.value.rgb.red = 1;
    reflection.specular_color.value.rgb.green = 1;
    reflection.specular_color.value.rgb.blue = 1;
    PEXSetReflectionAttributes(GfxDpy, Renderer, PEXOCRender, &reflection);

    if (reflection.transmission > 0.0) {
	if (RendererAttrs.hlhsr_mode == PEXHLHSRZBuffer) {
	    /* Make transparent primitives not update the Z buffer
		   so they won't completely obscure later transparent primitives. */
{
static int init=False, hsroff;
if(!init){init=True; hsroff=(getenv("HSROFF")!=NULL);}
if(hsroff) {
	RendererAttrs.hlhsr_mode = PEXHLHSROff;
	PEXChangeRenderer(GfxDpy, Renderer, PEXRAHLHSRMode, &RendererAttrs);
	}
}

	    RendererAttrs.hlhsr_mode = PEXHPHLHSRZBufferReadOnly;
	    PEXChangeRenderer(GfxDpy, Renderer, PEXRAHLHSRMode, &RendererAttrs);
	}
    } else {
	if (RendererAttrs.hlhsr_mode == PEXHPHLHSRZBufferReadOnly) {
	    /* Make opaque primitives update the Z buffer. */
	    RendererAttrs.hlhsr_mode = PEXHLHSRZBuffer;
	    PEXChangeRenderer(GfxDpy, Renderer, PEXRAHLHSRMode, &RendererAttrs);
	}
    }
}



void set_depthcue( int onoff )
{
    if (onoff) {
	PEXSetDepthCueIndex(GfxDpy, Renderer, PEXOCRender, 1);
    } else {
	PEXSetDepthCueIndex(GfxDpy, Renderer, PEXOCRender, 0);
    }
}



void set_line_width( double w )
{
    PEXSetLineWidth(GfxDpy, Renderer, PEXOCRender, w);
}




void set_pretty( int onoff )
{
}



void start_aa_pass( int n )
{
   /* nothing */
}



void end_aa_pass( int n )
{
   /* nothing */
}



/**********************************************************************/
/***                         Drawing Functions                      ***/
/**********************************************************************/



void draw_isosurface( int n,
#ifdef BIG_GFX
                      uint_4 *index,
#else
                      uint_2 *index,
#endif
                      int_2 verts[][3],
                      int_1 norms[][3],
                      unsigned int color )
{
    int i, j, k, remaining_points;
    PEXArrayOfVertex data;
    PEXArrayOfFacetData facet_data; /* a dummy */
    PEXVertexNormal vertex_data[VERTS_PER_CALL];
    float vscale = 1.0/VERTEX_SCALE;
    float nscale = 1.0/NORMAL_SCALE;

    enable_lighting(1);
    set_color( color );

    remaining_points = n;
    k = 0;
    data.normal = vertex_data;
    facet_data.normal = NULL;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
            j = index[k+i];
	    vertex_data[i].point.x = verts[j][0] * vscale;
	    vertex_data[i].point.y = verts[j][1] * vscale;
	    vertex_data[i].point.z = verts[j][2] * vscale;
	    vertex_data[i].normal.x = norms[j][0] * nscale;
	    vertex_data[i].normal.y = norms[j][1] * nscale;
	    vertex_data[i].normal.z = norms[j][2] * nscale;
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
                         PEXGANormal, PEXColorTypeRGB, facet_data,
                         VERTS_PER_CALL, data);
	k += VERTS_PER_CALL-2; /* Triangles overlap by two points. */
	remaining_points -= VERTS_PER_CALL-2;
    }
    if (remaining_points > 2) {
	for (i=0; i<remaining_points; i++) {
            j = index[k+i];
	    vertex_data[i].point.x = verts[j][0] * vscale;
	    vertex_data[i].point.y = verts[j][1] * vscale;
	    vertex_data[i].point.z = verts[j][2] * vscale;
	    vertex_data[i].normal.x = norms[j][0] * nscale;
	    vertex_data[i].normal.y = norms[j][1] * nscale;
	    vertex_data[i].normal.z = norms[j][2] * nscale;
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
                         PEXGANormal, PEXColorTypeRGB, facet_data,
                         remaining_points, data);
    }

    enable_lighting(0);
}


void draw_colored_isosurface( int n,
#ifdef BIG_GFX
                              uint_4 *index,
#else
                              uint_2 *index,
#endif
                              int_2 verts[][3],
                              int_1 norms[][3],
                              uint_1 color_indexes[],
                              unsigned int color_table[],
			      int alphavalue)
{
    int i, j, k, remaining_points;
    PEXArrayOfVertex data;
    PEXArrayOfFacetData facet_data; /* a dummy */
    PEXVertexRGBNormal vertex_data[VERTS_PER_CALL];
    float vscale = 1.0/VERTEX_SCALE;
    float nscale = 1.0/NORMAL_SCALE;

    enable_lighting(1);

#if TODO
    if (alphavalue== -1) {
      /* variable alpha in the mesh */
    }
    else {
      /* constant alpha */
    }
#endif /* TODO */

    remaining_points = n;
    k = 0;
    data.rgb_normal = vertex_data;
    facet_data.rgb_normal = NULL;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    unsigned int c;
            j = index[k+i];
	    c = color_table[color_indexes[j]];
	    vertex_data[i].point.x = verts[j][0] * vscale;
	    vertex_data[i].point.y = verts[j][1] * vscale;
	    vertex_data[i].point.z = verts[j][2] * vscale;
	    vertex_data[i].normal.x = norms[j][0] * nscale;
	    vertex_data[i].normal.y = norms[j][1] * nscale;
	    vertex_data[i].normal.z = norms[j][2] * nscale;
	    vertex_data[i].rgb.red = UNPACK_RED(c) * 1.0 / 255.0;
	    vertex_data[i].rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
	    vertex_data[i].rgb.blue = UNPACK_BLUE(c) * 1.0 / 255.0;
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
                         PEXGAColor|PEXGANormal, PEXColorTypeRGB, facet_data,
                         VERTS_PER_CALL, data);
	k += VERTS_PER_CALL-2; /* Triangles overlap by two points. */
	remaining_points -= VERTS_PER_CALL-2;
    }
    if (remaining_points > 2) {
	for (i=0; i<remaining_points; i++) {
	    unsigned int c;
            j = index[k+i];
	    c = color_table[color_indexes[j]];
	    vertex_data[i].point.x = verts[j][0] * vscale;
	    vertex_data[i].point.y = verts[j][1] * vscale;
	    vertex_data[i].point.z = verts[j][2] * vscale;
	    vertex_data[i].normal.x = norms[j][0] * nscale;
	    vertex_data[i].normal.y = norms[j][1] * nscale;
	    vertex_data[i].normal.z = norms[j][2] * nscale;
	    vertex_data[i].rgb.red = UNPACK_RED(c) * 1.0 / 255.0;
	    vertex_data[i].rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
	    vertex_data[i].rgb.blue = UNPACK_BLUE(c) * 1.0 / 255.0;
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
                         PEXGAColor|PEXGANormal, PEXColorTypeRGB, facet_data,
                         remaining_points, data);
    }

    enable_lighting(0);
}


void draw_triangle_strip( int n, int_2 verts[][3], int_1 norms[][3],
                          unsigned int color )
{
    register int i, index, remaining_points;
    PEXArrayOfVertex data;
    PEXArrayOfFacetData facet_data; /* a dummy */
    PEXVertexNormal vertex_data[VERTS_PER_CALL];
    float vscale = 1.0/VERTEX_SCALE;
    float nscale = 1.0/NORMAL_SCALE;

    set_color( color );

    remaining_points = n;
    index = 0;
    data.normal = vertex_data;
    facet_data.normal = NULL;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    vertex_data[i].point.x = verts[index+i][0] * vscale;
	    vertex_data[i].point.y = verts[index+i][1] * vscale;
	    vertex_data[i].point.z = verts[index+i][2] * vscale;
	    vertex_data[i].normal.x = norms[index+i][0] * nscale;
	    vertex_data[i].normal.y = norms[index+i][1] * nscale;
	    vertex_data[i].normal.z = norms[index+i][2] * nscale;
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
	    PEXGANormal, PEXColorTypeRGB, facet_data, VERTS_PER_CALL, data);
	index += VERTS_PER_CALL-2; /* Triangles overlap by two points. */
	remaining_points -= VERTS_PER_CALL-2;
    }
    if (remaining_points > 2) {
	for (i=0; i<remaining_points; i++) {
	    vertex_data[i].point.x = verts[index+i][0] * vscale;
	    vertex_data[i].point.y = verts[index+i][1] * vscale;
	    vertex_data[i].point.z = verts[index+i][2] * vscale;
	    vertex_data[i].normal.x = norms[index+i][0] * nscale;
	    vertex_data[i].normal.y = norms[index+i][1] * nscale;
	    vertex_data[i].normal.z = norms[index+i][2] * nscale;
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
	    PEXGANormal, PEXColorTypeRGB, facet_data, remaining_points, data);
    }
}



void draw_colored_triangle_strip( int n,
                                  int_2 verts[][3], int_1 norms[][3],
                                  uint_1 color_indexes[],
                                  unsigned int color_table[] )
{
    int i, j, k, remaining_points;
    PEXArrayOfVertex data;
    PEXArrayOfFacetData facet_data; /* a dummy */
    PEXVertexRGBNormal vertex_data[VERTS_PER_CALL];
    float vscale = 1.0/VERTEX_SCALE;
    float nscale = 1.0/NORMAL_SCALE;

    enable_lighting(1);

    remaining_points = n;
    k = 0;
    data.rgb_normal = vertex_data;
    facet_data.rgb_normal = NULL;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    unsigned int c;
            j = k + i;
	    c = color_table[color_indexes[j]];
	    vertex_data[i].point.x = verts[j][0] * vscale;
	    vertex_data[i].point.y = verts[j][1] * vscale;
	    vertex_data[i].point.z = verts[j][2] * vscale;
	    vertex_data[i].normal.x = norms[j][0] * nscale;
	    vertex_data[i].normal.y = norms[j][1] * nscale;
	    vertex_data[i].normal.z = norms[j][2] * nscale;
	    vertex_data[i].rgb.red = UNPACK_RED(c) * 1.0 / 255.0;
	    vertex_data[i].rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
	    vertex_data[i].rgb.blue = UNPACK_BLUE(c) * 1.0 / 255.0;
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
                         PEXGAColor|PEXGANormal, PEXColorTypeRGB, facet_data,
                         VERTS_PER_CALL, data);
	k += VERTS_PER_CALL-2; /* Triangles overlap by two points. */
	remaining_points -= VERTS_PER_CALL-2;
    }
    if (remaining_points > 2) {
	for (i=0; i<remaining_points; i++) {
	    unsigned int c;
            j = k+i;
	    c = color_table[color_indexes[j]];
	    vertex_data[i].point.x = verts[j][0] * vscale;
	    vertex_data[i].point.y = verts[j][1] * vscale;
	    vertex_data[i].point.z = verts[j][2] * vscale;
	    vertex_data[i].normal.x = norms[j][0] * nscale;
	    vertex_data[i].normal.y = norms[j][1] * nscale;
	    vertex_data[i].normal.z = norms[j][2] * nscale;
	    vertex_data[i].rgb.red = UNPACK_RED(c) * 1.0 / 255.0;
	    vertex_data[i].rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
	    vertex_data[i].rgb.blue = UNPACK_BLUE(c) * 1.0 / 255.0;
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
                         PEXGAColor|PEXGANormal, PEXColorTypeRGB, facet_data,
                         remaining_points, data);
    }

    enable_lighting(0);
}


void draw_color_quadmesh( int rows, int columns, int_2 verts[][3],
                          uint_1 color_indexes[], unsigned int color_table[],
                          int alphavalue )
{
  int i, j, base1, base2, size, temp;
  unsigned int color_row1[1000];
  unsigned int color_row2[1000];
  unsigned int *row1ptr, *row2ptr, *tmp;
  PEXArrayOfVertex data;
  PEXArrayOfFacetData facet_data; /* a dummy */
  static PEXVertexRGB *vertex_data = NULL;
  static int max_size = 0;
  float vscale = 1.0/VERTEX_SCALE;

  enable_lighting(0);

  /* Swap rows and columns to match PEXlib expectations. */
  temp = rows;
  rows = columns;
  columns = temp;

    size = rows*columns;
    if (max_size < size) {
	max_size = size;
	vertex_data = (PEXVertexRGB *)
	    realloc(vertex_data, size*sizeof(PEXVertexRGB));
	if (vertex_data == 0) {
	    printf("Unable to allocate quad mesh buffer\n");
	    exit(-1);
	}
    }

    for (i=0; i<size; i++) {
      unsigned int c;
	vertex_data[i].point.x = verts[i][0] * vscale;
	vertex_data[i].point.y = verts[i][1] * vscale;
	vertex_data[i].point.z = verts[i][2] * vscale;
	c = color_table[color_indexes[i]];
	vertex_data[i].rgb.red   = UNPACK_RED(c) * 1.0 / 255.0;
	vertex_data[i].rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
	vertex_data[i].rgb.blue  = UNPACK_BLUE(c) * 1.0 / 255.0;
    }
    data.rgb = vertex_data;
    facet_data.normal = NULL;
    PEXQuadrilateralMesh(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex,
                         PEXGANone, PEXGAColor, PEXColorTypeRGB, facet_data,
                         rows, columns, data);
}


void draw_lit_color_quadmesh( int rows, int columns,
                              float verts[][3],
                              float norms[][3],
                              uint_1 color_indexes[],
                              unsigned int color_table[] )
{
  int i, j, base1, base2, size, temp;
  unsigned int color_row1[1000];
  unsigned int color_row2[1000];
  unsigned int *row1ptr, *row2ptr, *tmp;
  PEXArrayOfVertex data;
  PEXArrayOfFacetData facet_data; /* a dummy */
  static PEXVertexRGBNormal *vertex_data = NULL;
  static int max_size = 0;

  enable_lighting(1);

  /* Swap rows and columns to match PEXlib expectations. */
  temp = rows;
  rows = columns;
  columns = temp;

    size = rows*columns;
    if (max_size < size) {
	vertex_data = (PEXVertexRGBNormal *)
	    realloc(vertex_data, size*sizeof(PEXVertexRGBNormal));
	if (vertex_data == 0) {
	    printf("Unable to allocate quad mesh buffer\n");
	    exit(-1);
	}
    }
    if (vertex_data == 0) {
	printf("Unable to allocate quad mesh buffer\n");
	exit(-1);
    }

    for (i=0; i<size; i++) {
      unsigned int c;
        vertex_data[i].point.x = verts[i][0];
        vertex_data[i].point.y = verts[i][1];
        vertex_data[i].point.z = verts[i][2];
        vertex_data[i].normal.x = norms[i][0];
        vertex_data[i].normal.y = norms[i][1];
        vertex_data[i].normal.z = norms[i][2];
        c = color_table[color_indexes[i]];
        vertex_data[i].rgb.red   = UNPACK_RED(c) * 1.0 / 255.0;
        vertex_data[i].rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
        vertex_data[i].rgb.blue  = UNPACK_BLUE(c) * 1.0 / 255.0;
    }
    data.rgb_normal = vertex_data;
    facet_data.normal = NULL;
    PEXQuadrilateralMesh(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex,
                         PEXGANone, PEXGAColor|PEXGANormal,
                         PEXColorTypeRGB, facet_data,
                         rows, columns, data);
  enable_lighting(0);
}


void draw_wind_lines( int nvectors, int_2 verts[][3], unsigned int color )
{
   int i, j;
   PEXCoord vertex_data[3];
   float vscale = 1.0/VERTEX_SCALE;

   set_color( color );

   for (i=0;i<nvectors;i++) {
      j = i * 4;
      /* main vector */
      vertex_data[0].x = verts[j][0] * vscale;
      vertex_data[0].y = verts[j][1] * vscale;
      vertex_data[0].z = verts[j][2] * vscale;
      vertex_data[1].x = verts[j+1][0] * vscale;
      vertex_data[1].y = verts[j+1][1] * vscale;
      vertex_data[1].z = verts[j+1][2] * vscale;
      PEXPolyline(GfxDpy, Renderer, PEXOCRender, 2, vertex_data);
      /* head vectors */
      vertex_data[0].x = verts[j+2][0] * vscale;
      vertex_data[0].y = verts[j+2][1] * vscale;
      vertex_data[0].z = verts[j+2][2] * vscale;
      vertex_data[2].x = verts[j+3][0] * vscale;
      vertex_data[2].y = verts[j+3][1] * vscale;
      vertex_data[2].z = verts[j+3][2] * vscale;
      PEXPolyline(GfxDpy, Renderer, PEXOCRender, 3, vertex_data);
   }
}



void draw_disjoint_lines( int n, int_2 verts[][3], unsigned int color )
{
   int i;
   PEXCoord vertex_data[2];
   float vscale = 1.0/VERTEX_SCALE;

   set_color( color );

   for (i=0;i<n;i+=2 ) {
      vertex_data[0].x = verts[i][0] * vscale;
      vertex_data[0].y = verts[i][1] * vscale;
      vertex_data[0].z = verts[i][2] * vscale;
      vertex_data[1].x = verts[i+1][0] * vscale;
      vertex_data[1].y = verts[i+1][1] * vscale;
      vertex_data[1].z = verts[i+1][2] * vscale;
      PEXPolyline(GfxDpy, Renderer, PEXOCRender, 2, vertex_data);
   }
}




void draw_polylines( int n, int_2 verts[][3], unsigned int color )
{
    register int i, index, remaining_points;
    PEXCoord vertex_data[VERTS_PER_CALL];
    float vscale = 1.0/VERTEX_SCALE;

    set_color( color );

    remaining_points = n;
    index = 0;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    vertex_data[i].x = verts[index+i][0] * vscale;
	    vertex_data[i].y = verts[index+i][1] * vscale;
	    vertex_data[i].z = verts[index+i][2] * vscale;
	}
	PEXPolyline(GfxDpy, Renderer, PEXOCRender, VERTS_PER_CALL,
                    vertex_data);
	index += VERTS_PER_CALL-1; /* Polylines overlap by one point. */
	remaining_points -= VERTS_PER_CALL-1;
    }
    if (remaining_points > 1) {
	for (i=0; i<remaining_points; i++) {
	    vertex_data[i].x = verts[index+i][0] * vscale;
	    vertex_data[i].y = verts[index+i][1] * vscale;
	    vertex_data[i].z = verts[index+i][2] * vscale;
	}
	PEXPolyline(GfxDpy, Renderer, PEXOCRender, remaining_points,
                    vertex_data);
    }
}



void draw_colored_polylines( int n, int_2 verts[][3],
                             uint_1 color_indexes[],
                             unsigned int color_table[] )
{
    register int i, index, remaining_points;
    PEXVertexRGB vertex_data[VERTS_PER_CALL];
    float vscale = 1.0/VERTEX_SCALE;
    PEXListOfVertex polylines;

    polylines.vertices.rgb = vertex_data;

    remaining_points = n;
    index = 0;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    unsigned int color;
	    vertex_data[i].point.x = verts[index+i][0] * vscale;
	    vertex_data[i].point.y = verts[index+i][1] * vscale;
	    vertex_data[i].point.z = verts[index+i][2] * vscale;
	    color = color_table[color_indexes[index+i]];
	    vertex_data[i].rgb.red   = UNPACK_RED(color) * (1.0 / 255.0);
	    vertex_data[i].rgb.green = UNPACK_GREEN(color) * (1.0 / 255.0);
	    vertex_data[i].rgb.blue  = UNPACK_BLUE(color) * (1.0 / 255.0);
	}
	polylines.count = VERTS_PER_CALL;
	PEXPolylineSetWithData( GfxDpy, Renderer, PEXOCRender,
			        PEXGAColor, PEXColorTypeRGB,
				1, &polylines );

	index += VERTS_PER_CALL-1; /* Polylines overlap by one point. */
	remaining_points -= VERTS_PER_CALL-1;
    }
    if (remaining_points > 1) {
	for (i=0; i<remaining_points; i++) {
	    unsigned int color;
	    vertex_data[i].point.x = verts[index+i][0] * vscale;
	    vertex_data[i].point.y = verts[index+i][1] * vscale;
	    vertex_data[i].point.z = verts[index+i][2] * vscale;
	    color = color_table[color_indexes[index+i]];
	    vertex_data[i].rgb.red   = UNPACK_RED(color) * (1.0 / 255.0);
	    vertex_data[i].rgb.green = UNPACK_GREEN(color) * (1.0 / 255.0);
	    vertex_data[i].rgb.blue  = UNPACK_BLUE(color) * (1.0 / 255.0);
	}
	polylines.count = remaining_points;
	PEXPolylineSetWithData( GfxDpy, Renderer, PEXOCRender,
				PEXGAColor, PEXColorTypeRGB,
				1, &polylines );
    }
}


void draw_multi_lines( int n, float verts[][3], unsigned int color )
{
    register int i, index, remaining_points;
    PEXCoord vertex_data[VERTS_PER_CALL];
    int vcount;
    float vscale = 1.0/VERTEX_SCALE;

    set_color( color );

    vcount = 0;
    for (i=0;i<n;i++) {
       if (verts[i][0]==-999.0) {
          PEXPolyline(GfxDpy, Renderer, PEXOCRender, vcount, vertex_data);
          vcount = 0;
       }
       else {
          vertex_data[vcount].x = verts[i][0];
          vertex_data[vcount].y = verts[i][1];
          vertex_data[vcount].z = verts[i][2];
          vcount++;
          assert(vcount<=VERTS_PER_CALL);  /* if this fails this code */
                                           /* will need some work. */
       }
    }
}



void draw_cursor( Context current_ctx, int style, float x, float y, float z, unsigned int c )
{
   if (style == 1) {
	/* polygon-based cursor */
	PEXVector ShiftVector;
	PEXMatrix TranslateMatrix;
	static PEXCoord xa1[4] = {
	    { -0.05, -0.005,  0.005 },
	    { -0.05,  0.005, -0.005 },
	    {  0.05,  0.005, -0.005 },
	    {  0.05, -0.005,  0.005 },
	};
	static PEXCoord xa2[4] = {
	    { -0.05, -0.005, -0.005 },
	    { -0.05,  0.005,  0.005 },
	    {  0.05,  0.005,  0.005 },
	    {  0.05, -0.005, -0.005 },
	};
	static PEXCoord ya1[4] = {
	    { -0.005, -0.05,  0.005 },
	    {  0.005, -0.05, -0.005 },
	    {  0.005,  0.05, -0.005 },
	    { -0.005,  0.05,  0.005 },
	};
	static PEXCoord ya2[4] = {
	    { -0.005, -0.05, -0.005 },
	    {  0.005, -0.05,  0.005 },
	    {  0.005,  0.05,  0.005 },
	    { -0.005,  0.05, -0.005 },
	};
	static PEXCoord za1[4] = {
	    { -0.005, -0.005,  0.05 },
	    {  0.005,  0.005,  0.05 },
	    {  0.005,  0.005, -0.05 },
	    { -0.005, -0.005, -0.05 },
	};
	static PEXCoord za2[4] = {
	    { -0.005,  0.005,  0.05 },
	    {  0.005, -0.005,  0.05 },
	    {  0.005, -0.005, -0.05 },
	    { -0.005,  0.005, -0.05 },
	};

	ShiftVector.x = x; ShiftVector.y = y; ShiftVector.z = z;
	PEXTranslate(&ShiftVector, TranslateMatrix);
	PEXSetLocalTransform(GfxDpy, Renderer, PEXOCRender, PEXReplace,
	    TranslateMatrix);

	PEXFillArea(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex, False,
	    4, xa1);
	PEXFillArea(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex, False,
	    4, xa2);
	PEXFillArea(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex, False,
	    4, ya1);
	PEXFillArea(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex, False,
	    4, ya2);
	PEXFillArea(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex, False,
	    4, za1);
	PEXFillArea(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex, False,
	    4, za2);

	PEXIdentityMatrix(TranslateMatrix);
	PEXSetLocalTransform(GfxDpy, Renderer, PEXOCRender, PEXReplace,
	    TranslateMatrix);
   }
   if (style == 0 ) {
      /* line segment cursor */
      PEXCoord vertex_data[2];
      PEXColor color;

      color.rgb.red   = UNPACK_RED(c)   / 255.0;
      color.rgb.green = UNPACK_GREEN(c) / 255.0;
      color.rgb.blue  = UNPACK_BLUE(c)  / 255.0;
      PEXSetLineColor(GfxDpy, Renderer, PEXOCRender, PEXColorTypeRGB, &color);

      vertex_data[0].x = x-0.05;
      vertex_data[0].y = y;
      vertex_data[0].z = z;
      vertex_data[1].x = x+0.05;
      vertex_data[1].y = y;
      vertex_data[1].z = z;
      PEXPolyline( GfxDpy, Renderer, PEXOCRender, 2, vertex_data );

      vertex_data[0].x = x;
      vertex_data[0].y = y-0.05;
      vertex_data[0].z = z;
      vertex_data[1].x = x;
      vertex_data[1].y = y+0.05;
      vertex_data[1].z = z;
      PEXPolyline( GfxDpy, Renderer, PEXOCRender, 2, vertex_data );

      vertex_data[0].x = x;
      vertex_data[0].y = y;
      vertex_data[0].z = z-0.05;
      vertex_data[1].x = x;
      vertex_data[1].y = y;
      vertex_data[1].z = z+0.05;
      PEXPolyline( GfxDpy, Renderer, PEXOCRender, 2, vertex_data );
   }
   if (style == 2) {
      /* sounding cursor */
      PEXCoord vertex_data[2];
      PEXColor color;

      color.rgb.red   = UNPACK_RED(c)   / 255.0;
      color.rgb.green = UNPACK_GREEN(c) / 255.0;
      color.rgb.blue  = UNPACK_BLUE(c)  / 255.0;
      PEXSetLineColor(GfxDpy, Renderer, PEXOCRender, PEXColorTypeRGB, &color);

      vertex_data[0].x = x;
      vertex_data[0].y = y;
      vertex_data[0].z = z - current_ctx->Zmin;
      vertex_data[1].x = x;
      vertex_data[1].y = y;
      vertex_data[1].z = z + current_ctx->Zmin;
      PEXPolyline( GfxDpy, Renderer, PEXOCRender, 2, vertex_data );

      vertex_data[0].x = x-0.05;
      vertex_data[0].y = y;
      vertex_data[0].z = z + current_ctx->Zmax;
      vertex_data[1].x = x+0.05;
      vertex_data[1].y = y;
      vertex_data[1].z = z + current_ctx->Zmax;
      PEXPolyline( GfxDpy, Renderer, PEXOCRender, 2, vertex_data );

      vertex_data[0].x = x;
      vertex_data[0].y = y-0.05;
      vertex_data[0].z = z + current_ctx->Zmax;
      vertex_data[1].x = x;
      vertex_data[1].y = y+0.05;
      vertex_data[1].z = z + current_ctx->Zmax;
      PEXPolyline( GfxDpy, Renderer, PEXOCRender, 2, vertex_data );

   }
}


#ifdef JUNK
/*** polytrinorm ******************************************************
   Draw a polytriangle strip with normals.
**********************************************************************/
void polytrinorm( vert, norm, n )
float vert[][3];
float norm[][3];
int n;
{
    register int i, index, remaining_points;
    PEXArrayOfVertex data;
    PEXArrayOfFacetData facet_data; /* a dummy */
    PEXVertexNormal vertex_data[VERTS_PER_CALL];

    remaining_points = n;
    index = 0;
    data.normal = vertex_data;
    facet_data.normal = NULL;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    vertex_data[i].point.x = vert[index+i][0];
	    vertex_data[i].point.y = vert[index+i][1];
	    vertex_data[i].point.z = vert[index+i][2];
	    vertex_data[i].normal.x = norm[index+i][0];
	    vertex_data[i].normal.y = norm[index+i][1];
	    vertex_data[i].normal.z = norm[index+i][2];
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
	    PEXGANormal, PEXColorTypeRGB, facet_data, VERTS_PER_CALL, data);
	index += VERTS_PER_CALL-2; /* Triangles overlap by two points. */
	remaining_points -= VERTS_PER_CALL-2;
    }
    if (remaining_points > 2) {
	for (i=0; i<remaining_points; i++) {
	    vertex_data[i].point.x = vert[index+i][0];
	    vertex_data[i].point.y = vert[index+i][1];
	    vertex_data[i].point.z = vert[index+i][2];
	    vertex_data[i].normal.x = norm[index+i][0];
	    vertex_data[i].normal.y = norm[index+i][1];
	    vertex_data[i].normal.z = norm[index+i][2];
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
	    PEXGANormal, PEXColorTypeRGB, facet_data, remaining_points, data);
    }
}
#endif



#ifdef JUNK
/*** polytri **********************************************************
   Draw a polytriangle strip without shading.
**********************************************************************/
void polytri( vert, n )
float vert[][3];
int n;
{
    register int i, index, remaining_points;
    PEXArrayOfVertex data;
    PEXArrayOfFacetData facet_data; /* a dummy */
    PEXCoord vertex_data[VERTS_PER_CALL];

    remaining_points = n;
    index = 0;
    data.no_data = vertex_data;
    facet_data.normal = NULL;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    vertex_data[i].x = vert[index+i][0];
	    vertex_data[i].y = vert[index+i][1];
	    vertex_data[i].z = vert[index+i][2];
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
	    PEXGANone, PEXColorTypeRGB, facet_data, VERTS_PER_CALL, data);
	index += VERTS_PER_CALL-2; /* Triangles overlap by two points. */
	remaining_points -= VERTS_PER_CALL-2;
    }
    if (remaining_points > 2) {
	for (i=0; i<remaining_points; i++) {
	    vertex_data[i].x = vert[index+i][0];
	    vertex_data[i].y = vert[index+i][1];
	    vertex_data[i].z = vert[index+i][2];
	}
	PEXTriangleStrip(GfxDpy, Renderer, PEXOCRender, PEXGANone,
	    PEXGANone, PEXColorTypeRGB, facet_data, remaining_points, data);
    }
}
#endif


void polyline( float vert[][3], int n )
{
    register int i, index, remaining_points;
    PEXCoord vertex_data[VERTS_PER_CALL];

    remaining_points = n;
    index = 0;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    vertex_data[i].x = vert[index+i][0];
	    vertex_data[i].y = vert[index+i][1];
	    vertex_data[i].z = vert[index+i][2];
	}
	PEXPolyline(GfxDpy, Renderer, PEXOCRender, VERTS_PER_CALL, vertex_data);
	index += VERTS_PER_CALL-1; /* Polylines overlap by one point. */
	remaining_points -= VERTS_PER_CALL-1;
    }
    if (remaining_points > 1) {
	for (i=0; i<remaining_points; i++) {
	    vertex_data[i].x = vert[index+i][0];
	    vertex_data[i].y = vert[index+i][1];
	    vertex_data[i].z = vert[index+i][2];
	}
	PEXPolyline(GfxDpy, Renderer, PEXOCRender, remaining_points, vertex_data);
    }
}




void disjointpolyline( float vert[][3], int n )
{
    register int i;
    PEXCoord vertex_data[2];

    for (i=0; i<n; i+=2) {
	vertex_data[0].x = vert[i][0];
	vertex_data[0].y = vert[i][1];
	vertex_data[0].z = vert[i][2];
	vertex_data[1].x = vert[i+1][0];
	vertex_data[1].y = vert[i+1][1];
	vertex_data[1].z = vert[i+1][2];
	PEXPolyline(GfxDpy, Renderer, PEXOCRender, 2, vertex_data);
    }
}


#ifdef JUNK
/*** quadmesh *********************************************************
   Draw a quadrilateral mesh with colors at each vertex.
**********************************************************************/
void quadmesh( vert, color, rows, cols )
float vert[][3];
unsigned int color[];
int rows, cols;
{
    register int i;
    unsigned long c;
    PEXArrayOfVertex data;
    PEXArrayOfFacetData facet_data; /* a dummy */
    int size;
    static PEXVertexRGB *vertex_data = NULL;
    static int max_size = 0;
    int temp;

    /* Swap rows and columns to match PEXlib expectations. */
    temp = rows;
    rows = cols;
    cols = temp;

    size = rows*cols;
    if (max_size < size) {
	max_size = size;
	vertex_data = (PEXVertexRGB *)
	    realloc(vertex_data, size*sizeof(PEXVertexRGB));
	if (vertex_data == 0) {
	    printf("Unable to allocate quad mesh buffer\n");
	    exit(-1);
	}
    }

    for (i=0; i<size; i++) {
	vertex_data[i].point.x = vert[i][0];
	vertex_data[i].point.y = vert[i][1];
	vertex_data[i].point.z = vert[i][2];
	c = color[i];
	vertex_data[i].rgb.red   = UNPACK_RED(c) * 1.0 / 255.0;
	vertex_data[i].rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
	vertex_data[i].rgb.blue  = UNPACK_BLUE(c) * 1.0 / 255.0;
    }
    data.rgb = vertex_data;
    facet_data.normal = NULL;
    PEXQuadrilateralMesh(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex,
	PEXGANone, PEXGAColor, PEXColorTypeRGB, facet_data,
	rows, cols, data);
}
#endif


void quadmeshnorm( float vert[][3], float norm[][3], unsigned int color[],
                   int rows, int cols )
{
    int i, size, temp;
    PEXArrayOfVertex data;
    PEXArrayOfFacetData facet_data; /* a dummy */
    static PEXVertexRGBNormal *vertex_data = NULL;
    static int max_size = 0;

    enable_lighting(0);

    /* Swap rows and columns to match PEXlib expectations. */
    temp = rows;
    rows = cols;
    cols = temp;

    size = rows*cols;
    if (max_size < size) {
	max_size = size;
	vertex_data = (PEXVertexRGBNormal *)
	    realloc(vertex_data, size*sizeof(PEXVertexRGBNormal));
	if (vertex_data == 0) {
	    printf("Unable to allocate quad mesh buffer\n");
	    exit(-1);
	}
    }

    for (i=0; i<size; i++) {
        unsigned int c;
	vertex_data[i].point.x = vert[i][0];
	vertex_data[i].point.y = vert[i][1];
	vertex_data[i].point.z = vert[i][2];
	vertex_data[i].normal.x = norm[i][0];
	vertex_data[i].normal.y = norm[i][1];
	vertex_data[i].normal.z = norm[i][2];
	c = color[i];
	vertex_data[i].rgb.red   = UNPACK_RED(c) * 1.0 / 255.0;
	vertex_data[i].rgb.green = UNPACK_GREEN(c) * 1.0 / 255.0;
	vertex_data[i].rgb.blue  = UNPACK_BLUE(c) * 1.0 / 255.0;
    }
    data.rgb_normal = vertex_data;
    facet_data.normal = NULL;
    PEXQuadrilateralMesh(GfxDpy, Renderer, PEXOCRender, PEXShapeConvex,
	PEXGANone, PEXGAColor|PEXGANormal, PEXColorTypeRGB, facet_data,
	rows, cols, data);
}




/*** polyline2d *******************************************************
   Draw a 2-D poly line.  Coordinates are in pixels with the origin
   in the upper-left corner of the window.  NOTE:  vertices are shorts.
**********************************************************************/
void polyline2d( short vert[][2], int n )
{
    register int i, index, remaining_points;
    PEXCoord2D vertex_data[VERTS_PER_CALL];

    remaining_points = n;
    index = 0;
    while (remaining_points > VERTS_PER_CALL) {
	for (i=0; i<VERTS_PER_CALL; i++) {
	    vertex_data[i].x = vert[index+i][0];
	    vertex_data[i].y = vert[index+i][1];
	}
	PEXPolyline2D(GfxDpy, Renderer, PEXOCRender, VERTS_PER_CALL, vertex_data);
	index += VERTS_PER_CALL-1; /* Polylines overlap by one point. */
	remaining_points -= VERTS_PER_CALL-1;
    }
    if (remaining_points > 1) {
	for (i=0; i<remaining_points; i++) {
	    vertex_data[i].x = vert[index+i][0];
	    vertex_data[i].y = vert[index+i][1];
	}
	PEXPolyline2D(GfxDpy, Renderer, PEXOCRender, remaining_points,
	    vertex_data);
    }
}



void draw_text( int xpos, int ypos, char *str )
{
    PEXCoord2D origin;

    /* Don't invert text, instead compute inverted Y position. */
    if (InvertY)
	PEXSetGlobalTransform(GfxDpy, Renderer, PEXOCRender, IdMat);
    origin.x = xpos;
    origin.y = current_ctx->WinHeight - ypos;
    PEXText2D(GfxDpy, Renderer, PEXOCRender, &origin, strlen(str), str);
    if (InvertY)
	PEXSetGlobalTransform(GfxDpy, Renderer, PEXOCRender, InvertYMatrix);
}



int text_width( char *str )
{
    PEXStringData strings[1];
    PEXTextExtent *extents;
    int width;
    strings[0].length = strlen(str);
    strings[0].ch = str;
    extents = PEXQueryTextExtents(GfxDpy, Renderer, 1, PEXPathRight,
	1.0, 0.0, 9.0, PEXHAlignNormal, PEXVAlignNormal, 1, strings);
    if (extents == NULL) {
	printf("Unable to inquire text extent\n");
	exit(-1);
    }
    width = extents->upper_right.x - extents->lower_left.x + 1;
    XFree(extents);
    return width;
}



int begin_object( void )
{
   return 0;
}


void end_object( void )
{
}

void call_object( int obj )
{
}


void delete_object( int objnum )
{
}

