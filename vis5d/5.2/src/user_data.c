/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
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

#include	<stdio.h>
#include	<string.h>
#include	<time.h>
#include	"map.h"
#include <stdlib.h>

/* Vis5D includes */
#include	"globals.h"



char	user_file_name[1000];



/**********************************************************************/
/*****                 user data local routines                   *****/
/**********************************************************************/



static char*	user_data_check_name (Display_Context dtx, char *name,
                                      char *vis5d_name)

{

    char	*ptmp;



    if (strlen (name) == 0) return NULL;

    if (strstr (name, dtx->Path) == name)
    {
        ptmp = name + strlen (dtx->Path);
        if (strstr (ptmp,   "/") == ptmp) name = ptmp;
        if (strstr (ptmp,  "./") == ptmp) name = ptmp;
        if (strstr (ptmp, "../") == ptmp) name = ptmp;
    }
    if (strlen (name) == 0) return NULL;

    if ((ptmp = strrchr (name, '/')) == NULL)
        ptmp = name;
    else
        ptmp++;

    if (strcmp (ptmp, vis5d_name) == 0) return NULL;


    return name;
}



/**********************************************************************/
/*****                 user data access routines                  *****/
/**********************************************************************/



int	user_data_get_header (char file_name[], v5dstruct *v)

{

    int		i;
    char	str[256];
    long	ltime, reftime, fcstime;
    FILE	*file;
    struct tm	*gmt;



    strcpy (user_file_name, "");

    fprintf (stderr, "Reading user header file %s\n", file_name);
    if ((file = fopen (file_name, "r")) == NULL) return 0;

    strcpy (user_file_name, file_name);



    /* Read the grid parameters */

    fgets (str, sizeof (str), file);
    sscanf (str, "%d%d%d\n", &v->Nc, &v->Nr, &v->Nl[0]);

    v->Projection  = PROJ_GENERIC;
    v->ProjArgs[0] = v->Nr - 1;
    v->ProjArgs[1] = v->Nc - 1;
    v->ProjArgs[2] = 1;
    v->ProjArgs[3] = 1;

    v->VerticalSystem = VERT_NONEQUAL_MB;
    for (i = 0; i < v->Nl[0]; i++)
    {
        fgets (str, sizeof (str), file);
        sscanf (str, "%f", &v->VertArgs[i]);
        v->VertArgs[i] = pressure_to_height (v->VertArgs[i]);
    }



    /* Read the variable information */

    fgets (str, sizeof (str), file);
    sscanf (str, "%d", &v->NumVars);

    for (i = 0; i < v->NumVars; i++)
    {
        fgets (str, sizeof (str), file);
        sscanf (str, "%s%s%f%f",
                v->VarName[i], v->Units[i], &v->MinVal[i], &v->MaxVal[i]);

        v->Nl[i] = v->Nl[0];
    }



    /* Read the time information */

    fgets (str, sizeof (str), file);
    sscanf (str, "%d", &reftime);

    fgets (str, sizeof (str), file);
    sscanf (str, "%d", &v->NumTimes);

    for (i = 0; i < v->NumTimes; i++)
    {
        fgets (str, sizeof (str), file);
        sscanf (str, "%d", &fcstime);

        ltime           = reftime + fcstime;
        gmt             = gmtime ((time_t *) &ltime);
        v->DateStamp[i] = (gmt->tm_year *  1000) + (gmt->tm_yday + 1);
        v->TimeStamp[i] = (gmt->tm_hour * 10000) +
                          (gmt->tm_min  *   100) + gmt->tm_sec;
    }

    fclose (file);



    v->CompressMode = 4;


    return 1;
}



int	user_data_get_grid (v5dstruct *v, int itime, int ivar,
                            float *grid_data)

{

    int		i, ir, ic, il, nr, nc, nl;
    long	ndat, noff, ltmp;
    char	file_name[1000], *ptmp;
    float	*dat;
    FILE	*file;



    nr = v->Nr;
    nc = v->Nc;
    nl = v->Nl[ivar];



    strcpy (file_name, user_file_name);

    if (strlen (file_name) == 0) return 0;

    ptmp = strrchr (file_name, '.');
    if (ptmp == NULL) ptmp = file_name + strlen (file_name);
    sprintf (ptmp, "_%s.dat", v->VarName[ivar]);

    fprintf (stderr, "Reading user grid file %s\n", file_name);
    if ((file = fopen (file_name, "rb")) == NULL) return 0;



    ndat = nr * nc * nl;
    dat  = (float *) malloc (ndat * sizeof (float));
    if (dat == NULL) return 0;

    noff = ((ndat * sizeof (float)) + (2 * sizeof (long))) * itime;
    fseek (file, noff, SEEK_SET);
    fread (&ltmp, 1, sizeof (long), file); /* skip over reference time */
    fread (&ltmp, 1, sizeof (long), file); /* skip over forecast time */
    fread (dat, ndat, sizeof (float), file);



#define VD(ROW, COL, LEV)   grid_data[ (ROW) + (((COL) + (LEV) * nc) * nr) ]

    i = 0;
    for (il = 0; il < nl; il++)
    {
        for (ir = nr-1; ir >= 0; ir--)
        {
            for (ic = 0; ic < nc; ic++, i++)
            {
                if (dat[i] == -99999.0) dat[i] = MISSING;
                VD(ir, ic, il) = dat[i];
            }
        }
    }

    free (dat);


    return 1;
}



int	user_data_get_topo (Display_Context dtx, char topo_name[])

{

    int		i, ir, ic, nr, nc, ielev;
    long	ndat, ltmp;
    char	file_name[1000], *ptmp;
    float	*dat;
    FILE	*file;



    if ((ptmp = user_data_check_name (dtx, topo_name, TOPOFILE)) != NULL)
    {
        strcpy (file_name, ptmp);
    }
    else
    {
        if (strlen (user_file_name) == 0) return 0;

        strcpy (file_name, user_file_name);

        ptmp = strrchr (file_name, '.');
        if (ptmp == NULL) ptmp = file_name + strlen (file_name);
        sprintf (ptmp, "_TOPO.dat");
    }

    fprintf (stderr, "Reading user topo file %s\n", file_name);
    if ((file = fopen (file_name, "rb")) == NULL) return 0;



    nr   = dtx->Nr;
    nc   = dtx->Nc;
    ndat = nr * nc;
    dat  = (float *) malloc (ndat * sizeof (float));
    if (dat == NULL) return 0;

    fread (&ltmp, 1, sizeof (long), file); /* skip over reference time */
    fread (&ltmp, 1, sizeof (long), file); /* skip over forecast time */
    fread (dat, ndat, sizeof (float), file);



    dtx->TopoData = (short *) malloc (ndat * sizeof (short));
    if (dtx->TopoData == NULL)
    {
        free (dat);
        return 0;
    }


#define TD(ROW, COL)   dtx->TopoData[ (COL) + ((ROW) * nc) ]

    i = 0;
    for (ir = nr-1; ir >= 0; ir--)
    {
        for (ic = 0; ic < nc; ic++, i++)
        {
            ielev = dat[i];
            ielev = (ielev == 0) ? 1 : ielev * 2;
            TD(ir, ic) = ielev;
        }
    }

    free (dat);

    dtx->Topo_rows = nr;
    dtx->Topo_cols = nc;

    dtx->Topo_westlon  = dtx->WestBound;
    dtx->Topo_eastlon  = dtx->EastBound;
    dtx->Topo_northlat = dtx->NorthBound;
    dtx->Topo_southlat = dtx->SouthBound;


    return 1;
}



int	user_data_get_map (Display_Context dtx, char map_name[])

{

    int		ifrst, nvect, nvert;
    char	file_name[1000], *ptmp;
    double	x, y, xx, yy, xfac, yfac, ymax, zmin;
    FILE	*file;



    if (((ptmp = user_data_check_name (dtx, map_name, WORLDFILE)) != NULL) &&
        ((ptmp = user_data_check_name (dtx, map_name,   USAFILE)) != NULL))
    {
        strcpy (file_name, ptmp);
    }
    else
    {
        if (strlen (user_file_name) == 0) return 0;

        strcpy (file_name, user_file_name);

        ptmp = strrchr (file_name, '.');
        if (ptmp == NULL) ptmp = file_name + strlen (file_name);
        sprintf (ptmp, "_MAP.dat");
    }

    fprintf (stderr, "Reading user map file %s\n", file_name);
    if ((file = fopen (file_name, "rb")) == NULL) return 0;



    dtx->ClipMin0  = dtx->Xmin;
    dtx->ClipMax0  = dtx->Xmax;
    dtx->ClipMin1  = dtx->Ymin;
    dtx->ClipMax1  = dtx->Ymax;
    dtx->SegCount  = 0;
    dtx->VertCount = 0;

/*
 *  Our map coordinates are in topo grid column and row units, with
 *  the origin (0.0, 0.0) at the lower left.
 *
 *  Calculate the scale factors to convert between our coordinates
 *  and Vis5D volume box coordinates.
 */

    ymax = dtx->qrows - 1;
    xfac = (dtx->Xmax - dtx->Xmin) / ((double) (dtx->qcols - 1));
    yfac = (dtx->Ymin - dtx->Ymax) / ((double) (dtx->qrows - 1));

    zmin = dtx->Zmin + 0.01;

    while (fscanf (file, "%d%lf%lf", &ifrst, &x, &y) == 3)
    {
/*
 *  Our origin is at the lower-right; Vis5D topo origin is at upper-right.
 *  Flip our Y to match Vis5D topo coordinate scheme.
 */
        y = ymax - y;

        if (ifrst)
        {
            if (dtx->SegCount > 0)
                dtx->Len[dtx->SegCount-1] = dtx->VertCount -
                                            dtx->Start[dtx->SegCount-1];

            dtx->Start[dtx->SegCount] = dtx->VertCount;
            dtx->SegCount++;
        }

/*
 *  Convert our map coordinates to Vis5D volume box coordinates.
 */
        xx = (x * xfac) + dtx->Xmin;
        yy = (y * yfac) + dtx->Ymax;

        dtx->MapVert[dtx->VertCount][0]     = xx;
        dtx->MapVert[dtx->VertCount][1]     = yy;
        dtx->MapVert[dtx->VertCount][2]     = zmin;

        dtx->FlatMapVert[dtx->VertCount][0] = xx;
        dtx->FlatMapVert[dtx->VertCount][1] = yy;
        dtx->FlatMapVert[dtx->VertCount][2] = zmin;

        dtx->VertCount++;

        if (!ifrst) bend_map_seg_to_fit_topo (dtx);
    }

    if (dtx->SegCount > 0)
        dtx->Len[dtx->SegCount-1] = dtx->VertCount -
                                    dtx->Start[dtx->SegCount-1];

    fclose (file);


    return 1;
}
