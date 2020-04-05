/* eval2.c */

/*
 * Mesa 3-D graphics library
 * Version:  1.2
 * Copyright (C) 1995  Brian Paul  (brianp@ssec.wisc.edu)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
$Id: eval2.c,v 1.11 1996/02/26 15:06:12 brianp Exp $

$Log: eval2.c,v $
 * Revision 1.11  1996/02/26  15:06:12  brianp
 * removed dead code
 *
 * Revision 1.10  1996/02/14  15:40:34  brianp
 * replaced ROUND with ROUNDF
 *
 * Revision 1.9  1995/12/30  17:15:44  brianp
 * produce integer colors and color indexes instead of floats
 *
 * Revision 1.8  1995/11/09  16:57:28  brianp
 * recompute strides in glMap[12][df] functions per Johan Nouvel
 *
 * Revision 1.7  1995/11/03  17:40:12  brianp
 * removed unused variables
 *
 * Revision 1.6  1995/05/30  15:10:25  brianp
 * added glGetMap[dfi]v() functions
 *
 * Revision 1.5  1995/05/29  21:22:57  brianp
 * added glEvalCoord[12][df]v() functions
 *
 * Revision 1.4  1995/05/22  21:02:41  brianp
 * Release 1.2
 *
 * Revision 1.3  1995/05/12  19:26:43  brianp
 * replaced CC.Mode!=0 with INSIDE_BEGIN_END
 *
 * Revision 1.2  1995/03/04  19:29:44  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/03/03  16:03:16  brianp
 * Initial revision
 *
 */


/*
 * Version 2 of eval.c was written by
 * Bernd Barsuhn (bdbarsuh@cip.informatik.uni-erlangen.de) and
 * Volker Weiss (vrweiss@cip.informatik.uni-erlangen.de).
 *
 * My original implementation of evaluators was simplistic and didn't
 * compute surface normal vectors properly.  Bernd and Volker applied
 * used more sophisticated methods to get better results.
 *
 * Thanks guys!
 */



#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "context.h"
#include "draw.h"
#include "list.h"
#include "macros.h"


/*
 * Horner scheme for Bezier curves
 * 
 * Bezier curves can be computed via a Horner scheme.
 * Horner is numerically less stable than the de Casteljau
 * algorithm, but it is faster. For curves of degree n 
 * the complexity of Horner is O(n) and de Casteljau is O(n^2).
 * Since stability is not important for displaying curve 
 * points I decided to use the Horner scheme.
 *
 * A cubic Bezier curve with control points b0, b1, b2, b3 can be 
 * written as
 *
 *        (([3]        [3]     )     [3]       )     [3]
 * c(t) = (([0]*s*b0 + [1]*t*b1)*s + [2]*t^2*b2)*s + [3]*t^2*b3
 *
 *                                           [n]
 * where s=1-t and the binomial coefficients [i]. These can 
 * be computed iteratively using the identity:
 *
 * [n]               [n  ]             [n]
 * [i] = (n-i+1)/i * [i-1]     and     [0] = 1
 */

static void
horner_bezier_curve(GLfloat *cp, GLfloat *out, GLfloat t,
                    GLuint dim, GLuint order)
{
  GLfloat s, powert;
  GLuint i, k, bincoeff;

  if(order >= 2)
  { 
    bincoeff = order-1;
    s = 1.0-t;

    for(k=0; k<dim; k++)
      out[k] = s*cp[k] + bincoeff*t*cp[dim+k];

    for(i=2, cp+=2*dim, powert=t*t; i<order; i++, powert*=t, cp +=dim)
    {
      bincoeff *= order-i;
      bincoeff /= i;

      for(k=0; k<dim; k++)
        out[k] = s*out[k] + bincoeff*powert*cp[k];
    }
  }
  else /* order=1 -> constant curve */
  { 
    for(k=0; k<dim; k++)
      out[k] = cp[k];
  } 
}

/*
 * Tensor product Bezier surfaces
 *
 * Again the Horner scheme is used to compute a point on a 
 * TP Bezier surface. First a control polygon for a curve
 * on the surface in one parameter direction is computed,
 * then the point on the curve for the other parameter 
 * direction is evaluated.
 *
 * To store the curve control polygon additional storage
 * for max(uorder,vorder) points is needed in the 
 * control net cn.
 */

static void
horner_bezier_surf(GLfloat *cn, GLfloat *out, GLfloat u, GLfloat v,
                   GLuint dim, GLuint uorder, GLuint vorder)
{
  GLfloat *cp = cn + uorder*vorder*dim;
  GLuint i, uinc = vorder*dim;

  if(vorder > uorder)
  {
    if(uorder >= 2)
    { 
      GLfloat s, poweru;
      GLuint j, k, bincoeff;

      /* Compute the control polygon for the surface-curve in u-direction */
      for(j=0; j<vorder; j++)
      {
        GLfloat *ucp = &cn[j*dim];

        /* Each control point is the point for parameter u on a */ 
        /* curve defined by the control polygons in u-direction */
	bincoeff = uorder-1;
	s = 1.0-u;

	for(k=0; k<dim; k++)
	  cp[j*dim+k] = s*ucp[k] + bincoeff*u*ucp[uinc+k];

	for(i=2, ucp+=2*uinc, poweru=u*u; i<uorder; 
            i++, poweru*=u, ucp +=uinc)
	{
	  bincoeff *= uorder-i;
          bincoeff /= i;

	  for(k=0; k<dim; k++)
	    cp[j*dim+k] = s*cp[j*dim+k] + bincoeff*poweru*ucp[k];
	}
      }
        
      /* Evaluate curve point in v */
      horner_bezier_curve(cp, out, v, dim, vorder);
    }
    else /* uorder=1 -> cn defines a curve in v */
      horner_bezier_curve(cn, out, v, dim, vorder);
  }
  else /* vorder <= uorder */
  {
    if(vorder > 1)
    {
      GLuint i;

      /* Compute the control polygon for the surface-curve in u-direction */
      for(i=0; i<uorder; i++, cn += uinc)
      {
	/* For constant i all cn[i][j] (j=0..vorder) are located */
	/* on consecutive memory locations, so we can use        */
	/* horner_bezier_curve to compute the control points     */

	horner_bezier_curve(cn, &cp[i*dim], v, dim, vorder);
      }

      /* Evaluate curve point in u */
      horner_bezier_curve(cp, out, u, dim, uorder);
    }
    else  /* vorder=1 -> cn defines a curve in u */
      horner_bezier_curve(cn, out, u, dim, uorder);
  }
}

/*
 * The direct de Casteljau algorithm is used when a point on the
 * surface and the tangent directions spanning the tangent plane
 * should be computed (this is needed to compute normals to the
 * surface). In this case the de Casteljau algorithm approach is
 * nicer because a point and the partial derivatives can be computed 
 * at the same time. To get the correct tangent length du and dv
 * must be multiplied with the (u2-u1)/uorder-1 and (v2-v1)/vorder-1. 
 * Since only the directions are needed, this scaling step is omitted.
 *
 * De Casteljau needs additional storage for uorder*vorder
 * values in the control net cn.
 */

static void
de_casteljau_surf(GLfloat *cn, GLfloat *out, GLfloat *du, GLfloat *dv,
                  GLfloat u, GLfloat v, GLuint dim, 
                  GLuint uorder, GLuint vorder)
{
  GLfloat *dcn = cn + uorder*vorder*dim;
  GLfloat us = 1.0-u, vs = 1.0-v;
  GLuint h, i, j, k;
  GLuint minorder = uorder < vorder ? uorder : vorder;
  GLuint uinc = vorder*dim;
  GLuint dcuinc = vorder;
 
  /* Each component is evaluated separately to save buffer space  */
  /* This does not drasticaly decrease the performance of the     */
  /* algorithm. If additional storage for (uorder-1)*(vorder-1)   */
  /* points would be available, the components could be accessed  */
  /* in the innermost loop which could lead to less cache misses. */

#define CN(I,J,K) cn[(I)*uinc+(J)*dim+(K)] 
#define DCN(I, J) dcn[(I)*dcuinc+(J)]
  if(minorder < 3)
  {
    if(uorder==vorder)
    {
      for(k=0; k<dim; k++)
      {
	/* Derivative direction in u */
	du[k] = vs*(CN(1,0,k) - CN(0,0,k)) +
	         v*(CN(1,1,k) - CN(0,1,k));

	/* Derivative direction in v */
	dv[k] = us*(CN(0,1,k) - CN(0,0,k)) + 
	         u*(CN(1,1,k) - CN(1,0,k));

	/* bilinear de Casteljau step */
        out[k] =  us*(vs*CN(0,0,k) + v*CN(0,1,k)) +
	           u*(vs*CN(1,0,k) + v*CN(1,1,k));
      }
    }
    else if(minorder == uorder)
    {
      for(k=0; k<dim; k++)
      {
	/* bilinear de Casteljau step */
	DCN(1,0) =    CN(1,0,k) -   CN(0,0,k);
	DCN(0,0) = us*CN(0,0,k) + u*CN(1,0,k);

	for(j=0; j<vorder-1; j++)
	{
	  /* for the derivative in u */
	  DCN(1,j+1) =    CN(1,j+1,k) -   CN(0,j+1,k);
	  DCN(1,j)   = vs*DCN(1,j)    + v*DCN(1,j+1);

	  /* for the `point' */
	  DCN(0,j+1) = us*CN(0,j+1,k) + u*CN(1,j+1,k);
	  DCN(0,j)   = vs*DCN(0,j)    + v*DCN(0,j+1);
	}
        
	/* remaining linear de Casteljau steps until the second last step */
	for(h=minorder; h<vorder-1; h++)
	  for(j=0; j<vorder-h; j++)
	  {
	    /* for the derivative in u */
	    DCN(1,j) = vs*DCN(1,j) + v*DCN(1,j+1);

	    /* for the `point' */
	    DCN(0,j) = vs*DCN(0,j) + v*DCN(0,j+1);
	  }

	/* derivative direction in v */
	dv[k] = DCN(0,1) - DCN(0,0);

	/* derivative direction in u */
	du[k] =   vs*DCN(1,0) + v*DCN(1,1);

	/* last linear de Casteljau step */
	out[k] =  vs*DCN(0,0) + v*DCN(0,1);
      }
    }
    else /* minorder == vorder */
    {
      for(k=0; k<dim; k++)
      {
	/* bilinear de Casteljau step */
	DCN(0,1) =    CN(0,1,k) -   CN(0,0,k);
	DCN(0,0) = vs*CN(0,0,k) + v*CN(0,1,k);
	for(i=0; i<uorder-1; i++)
	{
	  /* for the derivative in v */
	  DCN(i+1,1) =    CN(i+1,1,k) -   CN(i+1,0,k);
	  DCN(i,1)   = us*DCN(i,1)    + u*DCN(i+1,1);

	  /* for the `point' */
	  DCN(i+1,0) = vs*CN(i+1,0,k) + v*CN(i+1,1,k);
	  DCN(i,0)   = us*DCN(i,0)    + u*DCN(i+1,0);
	}
        
	/* remaining linear de Casteljau steps until the second last step */
	for(h=minorder; h<uorder-1; h++)
	  for(i=0; i<uorder-h; i++)
	  {
	    /* for the derivative in v */
	    DCN(i,1) = us*DCN(i,1) + u*DCN(i+1,1);

	    /* for the `point' */
	    DCN(i,0) = us*DCN(i,0) + u*DCN(i+1,0);
	  }

	/* derivative direction in u */
	du[k] = DCN(1,0) - DCN(0,0);

	/* derivative direction in v */
	dv[k] =   us*DCN(0,1) + u*DCN(1,1);

	/* last linear de Casteljau step */
	out[k] =  us*DCN(0,0) + u*DCN(1,0);
      }
    }
  }
  else if(uorder == vorder)
  {
    for(k=0; k<dim; k++)
    {
      /* first bilinear de Casteljau step */
      for(i=0; i<uorder-1; i++)
      {
	DCN(i,0) = us*CN(i,0,k) + u*CN(i+1,0,k);
	for(j=0; j<vorder-1; j++)
	{
	  DCN(i,j+1) = us*CN(i,j+1,k) + u*CN(i+1,j+1,k);
	  DCN(i,j)   = vs*DCN(i,j)    + v*DCN(i,j+1);
	}
      }

      /* remaining bilinear de Casteljau steps until the second last step */
      for(h=2; h<minorder-1; h++)
	for(i=0; i<uorder-h; i++)
	{
	  DCN(i,0) = us*DCN(i,0) + u*DCN(i+1,0);
	  for(j=0; j<vorder-h; j++)
	  {
	    DCN(i,j+1) = us*DCN(i,j+1) + u*DCN(i+1,j+1);
	    DCN(i,j)   = vs*DCN(i,j)   + v*DCN(i,j+1);
	  }
	}

      /* derivative direction in u */
      du[k] = vs*(DCN(1,0) - DCN(0,0)) +
	       v*(DCN(1,1) - DCN(0,1));

      /* derivative direction in v */
      dv[k] = us*(DCN(0,1) - DCN(0,0)) + 
	       u*(DCN(1,1) - DCN(1,0));

      /* last bilinear de Casteljau step */
      out[k] =  us*(vs*DCN(0,0) + v*DCN(0,1)) +
	         u*(vs*DCN(1,0) + v*DCN(1,1));
    }
  }
  else if(minorder == uorder)
  {
    for(k=0; k<dim; k++)
    {
      /* first bilinear de Casteljau step */
      for(i=0; i<uorder-1; i++)
      {
	DCN(i,0) = us*CN(i,0,k) + u*CN(i+1,0,k);
	for(j=0; j<vorder-1; j++)
	{
	  DCN(i,j+1) = us*CN(i,j+1,k) + u*CN(i+1,j+1,k);
	  DCN(i,j)   = vs*DCN(i,j)    + v*DCN(i,j+1);
	}
      }

      /* remaining bilinear de Casteljau steps until the second last step */
      for(h=2; h<minorder-1; h++)
	for(i=0; i<uorder-h; i++)
	{
	  DCN(i,0) = us*DCN(i,0) + u*DCN(i+1,0);
	  for(j=0; j<vorder-h; j++)
	  {
	    DCN(i,j+1) = us*DCN(i,j+1) + u*DCN(i+1,j+1);
	    DCN(i,j)   = vs*DCN(i,j)   + v*DCN(i,j+1);
	  }
	}

      /* last bilinear de Casteljau step */
      DCN(2,0) =    DCN(1,0) -   DCN(0,0);
      DCN(0,0) = us*DCN(0,0) + u*DCN(1,0);
      for(j=0; j<vorder-1; j++)
      {
	/* for the derivative in u */
	DCN(2,j+1) =    DCN(1,j+1) -    DCN(0,j+1);
	DCN(2,j)   = vs*DCN(2,j)    + v*DCN(2,j+1);
	
	/* for the `point' */
	DCN(0,j+1) = us*DCN(0,j+1 ) + u*DCN(1,j+1);
	DCN(0,j)   = vs*DCN(0,j)    + v*DCN(0,j+1);
      }
        
      /* remaining linear de Casteljau steps until the second last step */
      for(h=minorder; h<vorder-1; h++)
	for(j=0; j<vorder-h; j++)
	{
	  /* for the derivative in u */
	  DCN(2,j) = vs*DCN(2,j) + v*DCN(2,j+1);
	  
	  /* for the `point' */
	  DCN(0,j) = vs*DCN(0,j) + v*DCN(0,j+1);
	}
      
      /* derivative direction in v */
      dv[k] = DCN(0,1) - DCN(0,0);
      
      /* derivative direction in u */
      du[k] =   vs*DCN(2,0) + v*DCN(2,1);
      
      /* last linear de Casteljau step */
      out[k] =  vs*DCN(0,0) + v*DCN(0,1);
    }
  }
  else /* minorder == vorder */
  {
    for(k=0; k<dim; k++)
    {
      /* first bilinear de Casteljau step */
      for(i=0; i<uorder-1; i++)
      {
	DCN(i,0) = us*CN(i,0,k) + u*CN(i+1,0,k);
	for(j=0; j<vorder-1; j++)
	{
	  DCN(i,j+1) = us*CN(i,j+1,k) + u*CN(i+1,j+1,k);
	  DCN(i,j)   = vs*DCN(i,j)    + v*DCN(i,j+1);
	}
      }

      /* remaining bilinear de Casteljau steps until the second last step */
      for(h=2; h<minorder-1; h++)
	for(i=0; i<uorder-h; i++)
	{
	  DCN(i,0) = us*DCN(i,0) + u*DCN(i+1,0);
	  for(j=0; j<vorder-h; j++)
	  {
	    DCN(i,j+1) = us*DCN(i,j+1) + u*DCN(i+1,j+1);
	    DCN(i,j)   = vs*DCN(i,j)   + v*DCN(i,j+1);
	  }
	}

      /* last bilinear de Casteljau step */
      DCN(0,2) =    DCN(0,1) -   DCN(0,0);
      DCN(0,0) = vs*DCN(0,0) + v*DCN(0,1);
      for(i=0; i<uorder-1; i++)
      {
	/* for the derivative in v */
	DCN(i+1,2) =    DCN(i+1,1)  -   DCN(i+1,0);
	DCN(i,2)   = us*DCN(i,2)    + u*DCN(i+1,2);
	
	/* for the `point' */
	DCN(i+1,0) = vs*DCN(i+1,0)  + v*DCN(i+1,1);
	DCN(i,0)   = us*DCN(i,0)    + u*DCN(i+1,0);
      }
      
      /* remaining linear de Casteljau steps until the second last step */
      for(h=minorder; h<uorder-1; h++)
	for(i=0; i<uorder-h; i++)
	{
	  /* for the derivative in v */
	  DCN(i,2) = us*DCN(i,2) + u*DCN(i+1,2);
	  
	  /* for the `point' */
	  DCN(i,0) = us*DCN(i,0) + u*DCN(i+1,0);
	}
      
      /* derivative direction in u */
      du[k] = DCN(1,0) - DCN(0,0);
      
      /* derivative direction in v */
      dv[k] =   us*DCN(0,2) + u*DCN(1,2);
      
      /* last linear de Casteljau step */
      out[k] =  us*DCN(0,0) + u*DCN(1,0);
    }
  }
#undef DCN
#undef CN
}

/*
 * Return the number of components per control point for any type of
 * evaluator.  Return 0 if bad target.
 */

static GLint components( GLenum target )
{
   switch (target) {
      case GL_MAP1_VERTEX_3:		return 3;
      case GL_MAP1_VERTEX_4:		return 4;
      case GL_MAP1_INDEX:		return 1;
      case GL_MAP1_COLOR_4:		return 4;
      case GL_MAP1_NORMAL:		return 3;
      case GL_MAP1_TEXTURE_COORD_1:	return 1;
      case GL_MAP1_TEXTURE_COORD_2:	return 2;
      case GL_MAP1_TEXTURE_COORD_3:	return 3;
      case GL_MAP1_TEXTURE_COORD_4:	return 4;
      case GL_MAP2_VERTEX_3:		return 3;
      case GL_MAP2_VERTEX_4:		return 4;
      case GL_MAP2_INDEX:		return 1;
      case GL_MAP2_COLOR_4:		return 4;
      case GL_MAP2_NORMAL:		return 3;
      case GL_MAP2_TEXTURE_COORD_1:	return 1;
      case GL_MAP2_TEXTURE_COORD_2:	return 2;
      case GL_MAP2_TEXTURE_COORD_3:	return 3;
      case GL_MAP2_TEXTURE_COORD_4:	return 4;
      default:				return 0;
   }
}



/*
 * Copy 1-parametric evaluator control points from user-specified 
 * memory space to a buffer of contiguous control points.
 * Input:  see glMap1f for details
 * Return:  pointer to buffer of contiguous control points or NULL if out
 *          of memory.
 */

static GLfloat *copy_points1_f( GLenum target,
			        GLint ustride, GLint uorder,
			        const GLfloat *points )
{
   GLfloat *buffer, *p;
   GLuint i, k, size = components(target);

   buffer = (GLfloat *) malloc(uorder * size * sizeof(GLfloat));

   if(buffer) 
      for(i=0, p=buffer; i<uorder; i++, points+=ustride)
	for(k=0; k<size; k++)
	  *p++ = points[k];

   return buffer;
}



/*
 * Same as above but convert doubles to floats.
 */

static GLfloat *copy_points1_d( GLenum target,
			        GLint ustride, GLint uorder,
			        const GLdouble *points )
{
   GLfloat *buffer, *p;
   GLuint i, k, size = components(target);

   buffer = (GLfloat *) malloc(uorder * size * sizeof(GLfloat));

   if(buffer)
      for(i=0, p=buffer; i<uorder; i++, points+=ustride)
	for(k=0; k<size; k++)
	  *p++ = (GLfloat) points[k];

   return buffer;
}

/*
 * Copy 2-parametric evaluator control points from user-specified 
 * memory space to a buffer of contiguous control points.
 * Additional memory is allocated to be used by the horner and
 * de Casteljau evaluation schemes.
 *
 * Input:  see glMap2f for details
 * Return:  pointer to buffer of contiguous control points or NULL if out
 *          of memory.
 */

static GLfloat *copy_points2_f( GLenum target,
			        GLint ustride, GLint uorder,
			        GLint vstride, GLint vorder,
			        const GLfloat *points )
{
   GLfloat *buffer, *p;
   GLuint i, j, k, size, dsize, hsize;
   GLint uinc;

   size = components(target);

   /* max(uorder, vorder) additional points are used in      */
   /* horner evaluation and uorder*vorder additional */
   /* values are needed for de Casteljau                     */
   dsize = (uorder == 2 && vorder == 2)? 0 : uorder*vorder;
   hsize = (uorder > vorder ? uorder : vorder)*size;

   if(hsize>dsize)
     buffer = (GLfloat *) malloc((uorder*vorder*size+hsize)*sizeof(GLfloat));
   else
     buffer = (GLfloat *) malloc((uorder*vorder*size+dsize)*sizeof(GLfloat));

   /* compute the increment value for the u-loop */
   uinc = ustride - vorder*vstride;

   if (buffer) 
      for (i=0, p=buffer; i<uorder; i++, points += uinc)
	 for (j=0; j<vorder; j++, points += vstride)
	    for (k=0; k<size; k++)
	       *p++ = points[k];

   return buffer;
}



/*
 * Same as above but convert doubles to floats.
 */

static GLfloat *copy_points2_d(GLenum target,
			       GLint ustride, GLint uorder,
			       GLint vstride, GLint vorder,
			       const GLdouble *points )
{
   GLfloat *buffer, *p;
   GLuint i, j, k, size, hsize, dsize;
   GLint uinc;

   size = components(target);

   /* max(uorder, vorder) additional points are used in      */
   /* horner evaluation and uorder*vorder additional */
   /* values are needed for de Casteljau                     */
   dsize = (uorder == 2 && vorder == 2)? 0 : uorder*vorder;
   hsize = (uorder > vorder ? uorder : vorder)*size;

   if(hsize>dsize)
     buffer = (GLfloat *) malloc((uorder*vorder*size+hsize)*sizeof(GLfloat));
   else
     buffer = (GLfloat *) malloc((uorder*vorder*size+dsize)*sizeof(GLfloat));

   /* compute the increment value for the u-loop */
   uinc = ustride - vorder*vstride;

   if (buffer) 
      for (i=0, p=buffer; i<uorder; i++, points += uinc)
	 for (j=0; j<vorder; j++, points += vstride)
	    for (k=0; k<size; k++)
	       *p++ = (GLfloat) points[k];

   return buffer;
}

/**********************************************************************/
/*                            Internal                                */
/**********************************************************************/


/*
 * Do one-time initialization for evaluators.
 */

void gl_init_eval( void )
{
  static int init_flag = 0;

  /* Compute a table of nCr (combination) values used by the
   * Bernstein polynomial generator.
   */

  if (init_flag==0) 
  { /* no initialization needed */ 
  }

  init_flag = 1;
}


/*
 * Control points and info are shared by all contexts in the address space.
 * The discard flag indicates whether the current control point data can be
 * free()'d when new control points are given via glMap[12][fd].  It can't
 * freed be when the current control points are also in a display list.
 */


/* Map 1, Vertex_3 */
static GLuint Map1Vertex3order;
static GLfloat Map1Vertex3u1, Map1Vertex3u2;
static GLfloat *Map1Vertex3 = NULL;
static GLboolean DiscardMap1Vertex3 = GL_FALSE;

/* Map 1, Vertex_4 */
static GLuint Map1Vertex4order;
static GLfloat Map1Vertex4u1, Map1Vertex4u2;
static GLfloat *Map1Vertex4 = NULL;
static GLboolean DiscardMap1Vertex4 = GL_FALSE;

/* Map 1, Index */
static GLuint Map1Indexorder;
static GLfloat Map1Indexu1, Map1Indexu2;
static GLfloat *Map1Index = NULL;
static GLboolean DiscardMap1Index = GL_FALSE;

/* Map 1, Color_4 */
static GLuint Map1Color4order;
static GLfloat Map1Color4u1, Map1Color4u2;
static GLfloat *Map1Color4 = NULL;
static GLboolean DiscardMap1Color4 = GL_FALSE;

/* Map 1, Normal */
static GLuint Map1Normalorder;
static GLfloat Map1Normalu1, Map1Normalu2;
static GLfloat *Map1Normal = NULL;
static GLboolean DiscardMap1Normal = GL_FALSE;

/* Map 1, Texture_1 */
static GLuint Map1Texture1order;
static GLfloat Map1Texture1u1, Map1Texture1u2;
static GLfloat *Map1Texture1 = NULL;
static GLboolean DiscardMap1Texture1 = GL_FALSE;

/* Map 1, Texture_2 */
static GLuint Map1Texture2order;
static GLfloat Map1Texture2u1, Map1Texture2u2;
static GLfloat *Map1Texture2 = NULL;
static GLboolean DiscardMap1Texture2 = GL_FALSE;

/* Map 1, Texture_3 */
static GLuint Map1Texture3order;
static GLfloat Map1Texture3u1, Map1Texture3u2;
static GLfloat *Map1Texture3 = NULL;
static GLboolean DiscardMap1Texture3 = GL_FALSE;

/* Map 1, Texture_4 */
static GLuint Map1Texture4order;
static GLfloat Map1Texture4u1, Map1Texture4u2;
static GLfloat *Map1Texture4 = NULL;
static GLboolean DiscardMap1Texture4 = GL_FALSE;


/* Map 2, Vertex_3 */
static GLuint Map2Vertex3uorder;
static GLuint Map2Vertex3vorder;
static GLfloat Map2Vertex3u1, Map2Vertex3u2;
static GLfloat Map2Vertex3v1, Map2Vertex3v2;
static GLfloat *Map2Vertex3 = NULL;
static GLboolean DiscardMap2Vertex3 = GL_FALSE;

/* Map 2, Vertex_4 */
static GLuint Map2Vertex4uorder;
static GLuint Map2Vertex4vorder;
static GLfloat Map2Vertex4u1, Map2Vertex4u2;
static GLfloat Map2Vertex4v1, Map2Vertex4v2;
static GLfloat *Map2Vertex4 = NULL;
static GLboolean DiscardMap2Vertex4 = GL_FALSE;

/* Map 2, Index */
static GLuint Map2Indexuorder;
static GLuint Map2Indexvorder;
static GLfloat Map2Indexu1, Map2Indexu2;
static GLfloat Map2Indexv1, Map2Indexv2;
static GLfloat *Map2Index = NULL;
static GLboolean DiscardMap2Index = GL_FALSE;

/* Map 2, Color_4 */
static GLuint Map2Color4uorder;
static GLuint Map2Color4vorder;
static GLfloat Map2Color4u1, Map2Color4u2;
static GLfloat Map2Color4v1, Map2Color4v2;
static GLfloat *Map2Color4 = NULL;
static GLboolean DiscardMap2Color4 = GL_FALSE;

/* Map 2, Normal */
static GLuint Map2Normaluorder;
static GLuint Map2Normalvorder;
static GLfloat Map2Normalu1, Map2Normalu2;
static GLfloat Map2Normalv1, Map2Normalv2;
static GLfloat *Map2Normal = NULL;
static GLboolean DiscardMap2Normal = GL_FALSE;

/* Map 2, Texture_1 */
static GLuint Map2Texture1uorder;
static GLuint Map2Texture1vorder;
static GLfloat Map2Texture1u1, Map2Texture1u2;
static GLfloat Map2Texture1v1, Map2Texture1v2;
static GLfloat *Map2Texture1 = NULL;
static GLboolean DiscardMap2Texture1 = GL_FALSE;

/* Map 2, Texture_2 */
static GLuint Map2Texture2uorder;
static GLuint Map2Texture2vorder;
static GLfloat Map2Texture2u1, Map2Texture2u2;
static GLfloat Map2Texture2v1, Map2Texture2v2;
static GLfloat *Map2Texture2 = NULL;
static GLboolean DiscardMap2Texture2 = GL_FALSE;

/* Map 2, Texture_3 */
static GLuint Map2Texture3uorder;
static GLuint Map2Texture3vorder;
static GLfloat Map2Texture3u1, Map2Texture3u2;
static GLfloat Map2Texture3v1, Map2Texture3v2;
static GLfloat *Map2Texture3 = NULL;
static GLboolean DiscardMap2Texture3 = GL_FALSE;

/* Map 2, Texture_4 */
static GLuint Map2Texture4uorder;
static GLuint Map2Texture4vorder;
static GLfloat Map2Texture4u1, Map2Texture4u2;
static GLfloat Map2Texture4v1, Map2Texture4v2;
static GLfloat *Map2Texture4 = NULL;
static GLboolean DiscardMap2Texture4 = GL_FALSE;

/*
 * Given a Map target, return a pointer to the corresponding Discard
 * variable.
 */
static GLboolean *discard_target( GLenum target )
{
   switch (target) {
      case GL_MAP1_VERTEX_3:		return &DiscardMap1Vertex3;
      case GL_MAP1_VERTEX_4:		return &DiscardMap1Vertex4;
      case GL_MAP1_INDEX:		return &DiscardMap1Index;
      case GL_MAP1_COLOR_4:		return &DiscardMap1Color4;
      case GL_MAP1_NORMAL:		return &DiscardMap1Normal;
      case GL_MAP1_TEXTURE_COORD_1:	return &DiscardMap1Texture1;
      case GL_MAP1_TEXTURE_COORD_2:	return &DiscardMap1Texture2;
      case GL_MAP1_TEXTURE_COORD_3:	return &DiscardMap1Texture3;
      case GL_MAP1_TEXTURE_COORD_4:	return &DiscardMap1Texture4;
      case GL_MAP2_VERTEX_3:		return &DiscardMap2Vertex3;
      case GL_MAP2_VERTEX_4:		return &DiscardMap2Vertex4;
      case GL_MAP2_INDEX:		return &DiscardMap2Index;
      case GL_MAP2_COLOR_4:		return &DiscardMap2Color4;
      case GL_MAP2_NORMAL:		return &DiscardMap2Normal;
      case GL_MAP2_TEXTURE_COORD_1:	return &DiscardMap2Texture1;
      case GL_MAP2_TEXTURE_COORD_2:	return &DiscardMap2Texture2;
      case GL_MAP2_TEXTURE_COORD_3:	return &DiscardMap2Texture3;
      case GL_MAP2_TEXTURE_COORD_4:	return &DiscardMap2Texture4;
      default:
	 gl_error( GL_INVALID_ENUM, "discard_target" );
	 return NULL;
   }
}

/**********************************************************************/
/*                              Public                                */
/**********************************************************************/

/*
 * This function is called by the display list deallocator function to
 * specify that a given set of control points are no longer needed.
 * Under certain conditions, we can deallocate the control points memory,
 * otherwise, we mark it as discard-able.
 */

void gl_free_control_points( GLenum target, GLfloat *data )
{
   switch (target) {
      case GL_MAP1_VERTEX_3:
         if (data==Map1Vertex3) {
	    /* The control points in the display list are currently */
	    /* being used so we can mark them as discard-able. */
	    DiscardMap1Vertex3 = GL_TRUE;
	 }
	 else {
	    /* The control points in the display list are not currently */
	    /* being used. */
	    free( data );
	 }
	 break;
      case GL_MAP1_VERTEX_4:
         if (data==Map1Vertex4)
	    DiscardMap1Vertex4 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP1_INDEX:
         if (data==Map1Index)
	    DiscardMap1Index = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP1_COLOR_4:
         if (data==Map1Vertex4)
	    DiscardMap1Vertex4 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP1_NORMAL:
         if (data==Map1Normal)
	    DiscardMap1Normal = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP1_TEXTURE_COORD_1:
         if (data==Map1Texture1)
	    DiscardMap1Texture1 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP1_TEXTURE_COORD_2:
         if (data==Map1Texture2)
	    DiscardMap1Texture2 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP1_TEXTURE_COORD_3:
         if (data==Map1Texture3)
	    DiscardMap1Texture3 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP1_TEXTURE_COORD_4:
         if (data==Map1Texture4)
	    DiscardMap1Texture4 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_VERTEX_3:
         if (data==Map2Vertex3)
	    DiscardMap2Vertex3 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_VERTEX_4:
         if (data==Map2Vertex4)
	    DiscardMap2Vertex4 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_INDEX:
         if (data==Map2Index)
	    DiscardMap2Index = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_COLOR_4:
         if (data==Map2Color4)
	    DiscardMap2Color4 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_NORMAL:
         if (data==Map2Normal)
	    DiscardMap2Normal = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_TEXTURE_COORD_1:
         if (data==Map2Texture1)
	    DiscardMap2Texture1 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_TEXTURE_COORD_2:
         if (data==Map2Texture2)
	    DiscardMap2Texture2 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_TEXTURE_COORD_3:
         if (data==Map2Texture3)
	    DiscardMap2Texture3 = GL_TRUE;
	 else
	    free( data );
	 break;
      case GL_MAP2_TEXTURE_COORD_4:
         if (data==Map2Texture4)
	    DiscardMap2Texture4 = GL_TRUE;
	 else
	    free( data );
	 break;
      default:
	 gl_error( GL_INVALID_ENUM, "gl_free_control_points" );
   }
}

/*
 * Internal glMap1{fd} function.  Note that points must be a contiguous
 * array of control points.
 */

void gl_map1( GLenum target, GLfloat u1, GLfloat u2, GLint stride,
	      GLint order, const GLfloat *points )
{
   GLuint k;

   if (INSIDE_BEGIN_END) {
      gl_error( GL_INVALID_OPERATION, "glMap1" );
      return;
   }

   if (u1==u2) {
      gl_error( GL_INVALID_VALUE, "glMap1(u1,u2)" );
      return;
   }

   if (order<1 || order>MAX_EVAL_ORDER) {
      gl_error( GL_INVALID_VALUE, "glMap1(order)" );
      return;
   }

   k = components( target );
   if (k==0) {
      gl_error( GL_INVALID_ENUM, "glMap1(target)" );
   }

   if (stride < k) {
      gl_error( GL_INVALID_VALUE, "glMap1(stride)" );
      return;
   }

   switch (target) {
      case GL_MAP1_VERTEX_3:
         Map1Vertex3order = order;
	 Map1Vertex3u1 = u1;
	 Map1Vertex3u2 = u2;
	 if (Map1Vertex3 && DiscardMap1Vertex3) {
	    free( Map1Vertex3 );
	 }
	 DiscardMap1Vertex3 = GL_FALSE;
	 Map1Vertex3 = (GLfloat *) points;
	 break;
      case GL_MAP1_VERTEX_4:
         Map1Vertex4order = order;
	 Map1Vertex4u1 = u1;
	 Map1Vertex4u2 = u2;
	 if (Map1Vertex4 && DiscardMap1Vertex4) {
	    free( Map1Vertex4 );
	 }
	 DiscardMap1Vertex4 = GL_FALSE;
	 Map1Vertex4 = (GLfloat *) points;
	 break;
      case GL_MAP1_INDEX:
         Map1Indexorder = order;
	 Map1Indexu1 = u1;
	 Map1Indexu2 = u2;
	 if (Map1Index && DiscardMap1Index) {
	    free( Map1Index );
	 }
	 DiscardMap1Index = GL_FALSE;
	 Map1Index = (GLfloat *) points;
	 break;
      case GL_MAP1_COLOR_4:
         Map1Color4order = order;
	 Map1Color4u1 = u1;
	 Map1Color4u2 = u2;
	 if (Map1Color4 && DiscardMap1Color4) {
	    free( Map1Color4 );
	 }
	 DiscardMap1Color4 = GL_FALSE;
	 Map1Color4 = (GLfloat *) points;
	 break;
      case GL_MAP1_NORMAL:
         Map1Normalorder = order;
	 Map1Normalu1 = u1;
	 Map1Normalu2 = u2;
	 if (Map1Normal && DiscardMap1Normal) {
	    free( Map1Normal );
	 }
	 DiscardMap1Normal = GL_FALSE;
	 Map1Normal = (GLfloat *) points;
	 break;
      case GL_MAP1_TEXTURE_COORD_1:
         Map1Texture1order = order;
	 Map1Texture1u1 = u1;
	 Map1Texture1u2 = u2;
	 if (Map1Texture1 && DiscardMap1Texture1) {
	    free( Map1Texture1 );
	 }
	 DiscardMap1Texture1 = GL_FALSE;
	 Map1Texture1 = (GLfloat *) points;
	 break;
      case GL_MAP1_TEXTURE_COORD_2:
         Map1Texture2order = order;
	 Map1Texture2u1 = u1;
	 Map1Texture2u2 = u2;
	 if (Map1Texture2 && DiscardMap1Texture2) {
	    free( Map1Texture2 );
	 }
	 DiscardMap1Texture2 = GL_FALSE;
	 Map1Texture2 = (GLfloat *) points;
	 break;
      case GL_MAP1_TEXTURE_COORD_3:
         Map1Texture3order = order;
	 Map1Texture3u1 = u1;
	 Map1Texture3u2 = u2;
	 if (Map1Texture3 && DiscardMap1Texture3) {
	    free( Map1Texture3 );
	 }
	 DiscardMap1Texture3 = GL_FALSE;
	 Map1Texture3 = (GLfloat *) points;
	 break;
      case GL_MAP1_TEXTURE_COORD_4:
         Map1Texture4order = order;
	 Map1Texture4u1 = u1;
	 Map1Texture4u2 = u2;
	 if (Map1Texture4 && DiscardMap1Texture4) {
	    free( Map1Texture4 );
	 }
	 DiscardMap1Texture4 = GL_FALSE;
	 Map1Texture4 = (GLfloat *) points;
	 break;
      default:
         gl_error( GL_INVALID_ENUM, "glMap1(target)" );
   }
}

void glMap1f( GLenum target, GLfloat u1, GLfloat u2, GLint stride,
	      GLint order, const GLfloat *points )
{
   float *p;

   p = copy_points1_f(target, stride, order, points);

   if (!p) {
      gl_error( GL_OUT_OF_MEMORY, "glMap1f" );
      return;
   }

   /* may be a new stride after copying control points */
   stride = components( target );

   if (CC.CompileFlag) {
      gl_save_map1( target, u1, u2, stride, order, p );
   }
   if (CC.ExecuteFlag) {
      gl_map1( target, u1, u2, stride, order, p );
      if (!CC.CompileFlag) {
	 /* get pointer to the discard flag for the given target */
	 GLboolean *discard = discard_target( target );
  	 /* the control points can be discarded when new ones are bound */
	 *discard = GL_TRUE;
      }
   }
}

void glMap1d( GLenum target, GLdouble u1, GLdouble u2, GLint stride,
	      GLint order, const GLdouble *points )
{
   float *p;

   p = copy_points1_d(target, stride, order, points);

   if (!p) {
      gl_error( GL_OUT_OF_MEMORY, "glMap1d" );
      return;
   }

   /* may be a new stride after copying control points */
   stride = components( target );

   if (CC.CompileFlag) {
      gl_save_map1( target, u1, u2, stride, order, p );
   }
   if (CC.ExecuteFlag) {
      gl_map1( target, u1, u2, stride, order, p );
      if (!CC.CompileFlag) {
	 /* get pointer to the discard flag for the given target */
	 GLboolean *discard = discard_target( target );
  	 /* the control points can be discarded when new ones are bound */
	 *discard = GL_TRUE;
      }
   }
}




void gl_map2( GLenum target,
	      GLfloat u1, GLfloat u2, GLint ustride, GLint uorder,
	      GLfloat v1, GLfloat v2, GLint vstride, GLint vorder,
	      const GLfloat *points )
{
   GLuint k;

   if (INSIDE_BEGIN_END) {
      gl_error( GL_INVALID_OPERATION, "glMap2" );
      return;
   }

   if (u1==u2) {
      gl_error( GL_INVALID_VALUE, "glMap2(u1,u2)" );
      return;
   }

   if (v1==v2) {
      gl_error( GL_INVALID_VALUE, "glMap2(v1,v2)" );
      return;
   }

   if (uorder<1 || uorder>MAX_EVAL_ORDER) {
      gl_error( GL_INVALID_VALUE, "glMap2(uorder)" );
      return;
   }

   if (vorder<1 || vorder>MAX_EVAL_ORDER) {
      gl_error( GL_INVALID_VALUE, "glMap2(vorder)" );
      return;
   }

   k = components( target );
   if (k==0) {
      gl_error( GL_INVALID_ENUM, "glMap2(target)" );
   }

   if (ustride < k) {
      gl_error( GL_INVALID_VALUE, "glMap2(ustride)" );
      return;
   }
   if (vstride < k) {
      gl_error( GL_INVALID_VALUE, "glMap2(vstride)" );
      return;
   }

   switch (target) {
      case GL_MAP2_VERTEX_3:
         Map2Vertex3uorder = uorder;
	 Map2Vertex3u1 = u1;
	 Map2Vertex3u2 = u2;
         Map2Vertex3vorder = vorder;
	 Map2Vertex3v1 = v1;
	 Map2Vertex3v2 = v2;
	 if (Map2Vertex3 && DiscardMap2Vertex3) {
	    free( Map2Vertex3 );
	 }
	 DiscardMap2Vertex3 = GL_FALSE;
	 Map2Vertex3 = (GLfloat *) points;
	 break;
      case GL_MAP2_VERTEX_4:
         Map2Vertex4uorder = uorder;
	 Map2Vertex4u1 = u1;
	 Map2Vertex4u2 = u2;
         Map2Vertex4vorder = vorder;
	 Map2Vertex4v1 = v1;
	 Map2Vertex4v2 = v2;
	 if (Map2Vertex4 && DiscardMap2Vertex4) {
	    free( Map2Vertex4 );
	 }
	 DiscardMap2Vertex4 = GL_FALSE;
	 Map2Vertex4 = (GLfloat *) points;
	 break;
      case GL_MAP2_INDEX:
         Map2Indexuorder = uorder;
	 Map2Indexu1 = u1;
	 Map2Indexu2 = u2;
         Map2Indexvorder = vorder;
	 Map2Indexv1 = v1;
	 Map2Indexv2 = v2;
	 if (Map2Index && DiscardMap2Index) {
	    free( Map2Index );
	 }
	 DiscardMap2Index = GL_FALSE;
	 Map2Index = (GLfloat *) points;
	 break;
      case GL_MAP2_COLOR_4:
         Map2Color4uorder = uorder;
	 Map2Color4u1 = u1;
	 Map2Color4u2 = u2;
         Map2Color4vorder = vorder;
	 Map2Color4v1 = v1;
	 Map2Color4v2 = v2;
	 if (Map2Color4 && DiscardMap2Color4) {
	    free( Map2Color4 );
	 }
	 DiscardMap2Color4 = GL_FALSE;
	 Map2Color4 = (GLfloat *) points;
	 break;
      case GL_MAP2_NORMAL:
         Map2Normaluorder = uorder;
	 Map2Normalu1 = u1;
	 Map2Normalu2 = u2;
         Map2Normalvorder = vorder;
	 Map2Normalv1 = v1;
	 Map2Normalv2 = v2;
	 if (Map2Normal && DiscardMap2Normal) {
	    free( Map2Normal );
	 }
	 DiscardMap2Normal = GL_FALSE;
	 Map2Normal = (GLfloat *) points;
	 break;
      case GL_MAP2_TEXTURE_COORD_1:
         Map2Texture1uorder = uorder;
	 Map2Texture1u1 = u1;
	 Map2Texture1u2 = u2;
         Map2Texture1vorder = vorder;
	 Map2Texture1v1 = v1;
	 Map2Texture1v2 = v2;
	 if (Map2Texture1 && DiscardMap2Texture1) {
	    free( Map2Texture1 );
	 }
	 DiscardMap2Texture1 = GL_FALSE;
	 Map2Texture1 = (GLfloat *) points;
	 break;
      case GL_MAP2_TEXTURE_COORD_2:
         Map2Texture2uorder = uorder;
	 Map2Texture2u1 = u1;
	 Map2Texture2u2 = u2;
         Map2Texture2vorder = vorder;
	 Map2Texture2v1 = v1;
	 Map2Texture2v2 = v2;
	 if (Map2Texture2 && DiscardMap2Texture2) {
	    free( Map2Texture2 );
	 }
	 DiscardMap2Texture2 = GL_FALSE;
	 Map2Texture2 = (GLfloat *) points;
	 break;
      case GL_MAP2_TEXTURE_COORD_3:
         Map2Texture3uorder = uorder;
	 Map2Texture3u1 = u1;
	 Map2Texture3u2 = u2;
         Map2Texture3vorder = vorder;
	 Map2Texture3v1 = v1;
	 Map2Texture3v2 = v2;
	 if (Map2Texture3 && DiscardMap2Texture3) {
	    free( Map2Texture3 );
	 }
	 DiscardMap2Texture3 = GL_FALSE;
	 Map2Texture3 = (GLfloat *) points;
	 break;
      case GL_MAP2_TEXTURE_COORD_4:
         Map2Texture4uorder = uorder;
	 Map2Texture4u1 = u1;
	 Map2Texture4u2 = u2;
         Map2Texture4vorder = vorder;
	 Map2Texture4v1 = v1;
	 Map2Texture4v2 = v2;
	 if (Map2Texture4 && DiscardMap2Texture4) {
	    free( Map2Texture4 );
	 }
	 DiscardMap2Texture4 = GL_FALSE;
	 Map2Texture4 = (GLfloat *) points;
	 break;
      default:
         gl_error( GL_INVALID_ENUM, "glMap1f(target)" );
   }

}


   
void glMap2f( GLenum target,
	      GLfloat u1, GLfloat u2, GLint ustride, GLint uorder,
	      GLfloat v1, GLfloat v2, GLint vstride, GLint vorder,
	      const GLfloat *points )
{
   GLfloat *p;

   p = copy_points2_f(target, ustride, uorder, vstride, vorder, points);

   if (!p) {
      gl_error( GL_OUT_OF_MEMORY, "glMap2f" );
      return;
   }

   /* may be a new strides after copying control points */
   vstride = components( target );
   ustride = vorder * vstride;

   if (CC.CompileFlag) {
      gl_save_map2( target, u1, u2, ustride, uorder,
		    v1, v2, vstride, vorder, p );
   }
   if (CC.ExecuteFlag) {
      gl_map2( target, u1, u2, ustride, uorder,
	       v1, v2, vstride, vorder, p );
      if (!CC.CompileFlag) {
	 /* get pointer to the discard flag for the given target */
	 GLboolean *discard = discard_target( target );
  	 /* the control points can be discarded when new ones are bound */
	 *discard = GL_TRUE;
      }
   }
}



void glMap2d( GLenum target,
	      GLdouble u1, GLdouble u2, GLint ustride, GLint uorder,
	      GLdouble v1, GLdouble v2, GLint vstride, GLint vorder,
	      const GLdouble *points )
{
   GLfloat *p;

   p = copy_points2_d(target, ustride, uorder, vstride, vorder, points);

   if (!p) {
      gl_error( GL_OUT_OF_MEMORY, "glMap2d" );
      return;
   }

   /* may be a new strides after copying control points */
   vstride = components( target );
   ustride = vorder * vstride;

   if (CC.CompileFlag) {
      gl_save_map2( target, u1, u2, ustride, uorder,
		    v1, v2, vstride, vorder, p );
   }
   if (CC.ExecuteFlag) {
      gl_map2( target, u1, u2, ustride, uorder,
	       v1, v2, vstride, vorder, p );
      if (!CC.CompileFlag) {
	 /* get pointer to the discard flag for the given target */
	 GLboolean *discard = discard_target( target );
  	 /* the control points can be discarded when new ones are bound */
	 *discard = GL_TRUE;
      }
   }
}



void glGetMapdv( GLenum target, GLenum query, GLdouble *v )
{
   GLuint i, n;
   GLfloat *data;

   switch (query) {
      case GL_COEFF:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       data = Map1Color4;
	       n = Map1Color4order * 4;
	       break;
	    case GL_MAP1_INDEX:
	       data = Map1Index;
	       n = Map1Indexorder;
	       break;
	    case GL_MAP1_NORMAL:
	       data = Map1Normal;
	       n = Map1Normalorder * 3;
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       data = Map1Texture1;
	       n = Map1Texture1order * 1;
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       data = Map1Texture2;
	       n = Map1Texture2order * 2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       data = Map1Texture3;
	       n = Map1Texture3order * 3;
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       data = Map1Texture4;
	       n = Map1Texture4order * 4;
	       break;
	    case GL_MAP1_VERTEX_3:
	       data = Map1Vertex3;
	       n = Map1Vertex3order * 3;
	       break;
	    case GL_MAP1_VERTEX_4:
	       data = Map1Vertex4;
	       n = Map1Vertex4order * 4;
	       break;
	    case GL_MAP2_COLOR_4:
	       data = Map2Color4;
	       n = Map2Color4uorder * Map2Color4vorder * 4;
	       break;
	    case GL_MAP2_INDEX:
	       data = Map2Index;
	       n = Map2Indexuorder * Map2Indexvorder;
	       break;
	    case GL_MAP2_NORMAL:
	       data = Map2Normal;
	       n = Map2Normaluorder * Map2Normalvorder * 3;
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       data = Map2Texture1;
	       n = Map2Texture1uorder * Map2Texture1vorder * 1;
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       data = Map2Texture2;
	       n = Map2Texture2uorder * Map2Texture2vorder * 2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       data = Map2Texture3;
	       n = Map2Texture3uorder * Map2Texture3vorder * 3;
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       data = Map2Texture4;
	       n = Map2Texture4uorder * Map2Texture4vorder * 4;
	       break;
	    case GL_MAP2_VERTEX_3:
	       data = Map2Vertex3;
	       n = Map2Vertex3uorder * Map2Vertex3vorder * 3;
	       break;
	    case GL_MAP2_VERTEX_4:
	       data = Map2Vertex4;
	       n = Map2Vertex4uorder * Map2Vertex4vorder * 4;
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapdv(target)" );
	 }
	 if (data) {
	    for (i=0;i<n;i++) {
	       v[i] = data[i];
	    }
	 }
         break;
      case GL_ORDER:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       *v = Map1Color4order;
	       break;
	    case GL_MAP1_INDEX:
	       *v = Map1Indexorder;
	       break;
	    case GL_MAP1_NORMAL:
	       *v = Map1Normalorder;
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       *v = Map1Texture1order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       *v = Map1Texture2order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       *v = Map1Texture3order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       *v = Map1Texture4order;
	       break;
	    case GL_MAP1_VERTEX_3:
	       *v = Map1Vertex3order;
	       break;
	    case GL_MAP1_VERTEX_4:
	       *v = Map1Vertex4order;
	       break;
	    case GL_MAP2_COLOR_4:
	       v[0] = Map2Color4uorder;
	       v[1] = Map2Color4vorder;
	       break;
	    case GL_MAP2_INDEX:
	       v[0] = Map2Indexuorder;
	       v[1] = Map2Indexvorder;
	       break;
	    case GL_MAP2_NORMAL:
	       v[0] = Map2Normaluorder;
	       v[1] = Map2Normalvorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       v[0] = Map2Texture1uorder;
	       v[1] = Map2Texture1vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       v[0] = Map2Texture2uorder;
	       v[1] = Map2Texture2vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       v[0] = Map2Texture3uorder;
	       v[1] = Map2Texture3vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       v[0] = Map2Texture4uorder;
	       v[1] = Map2Texture4vorder;
	       break;
	    case GL_MAP2_VERTEX_3:
	       v[0] = Map2Vertex3uorder;
	       v[1] = Map2Vertex3vorder;
	       break;
	    case GL_MAP2_VERTEX_4:
	       v[0] = Map2Vertex4uorder;
	       v[1] = Map2Vertex4vorder;
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapdv(target)" );
	 }
         break;
      case GL_DOMAIN:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       v[0] = Map1Color4u1;
	       v[1] = Map1Color4u2;
	       break;
	    case GL_MAP1_INDEX:
	       v[0] = Map1Indexu1;
	       v[1] = Map1Indexu2;
	       break;
	    case GL_MAP1_NORMAL:
	       v[0] = Map1Normalu1;
	       v[1] = Map1Normalu2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       v[0] = Map1Texture1u1;
	       v[1] = Map1Texture1u2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       v[0] = Map1Texture2u1;
	       v[1] = Map1Texture2u2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       v[0] = Map1Texture3u1;
	       v[1] = Map1Texture3u2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       v[0] = Map1Texture4u1;
	       v[1] = Map1Texture4u2;
	       break;
	    case GL_MAP1_VERTEX_3:
	       v[0] = Map1Vertex3u1;
	       v[1] = Map1Vertex3u2;
	       break;
	    case GL_MAP1_VERTEX_4:
	       v[0] = Map1Vertex4u1;
	       v[1] = Map1Vertex4u2;
	       break;
	    case GL_MAP2_COLOR_4:
	       v[0] = Map2Color4u1;
	       v[1] = Map2Color4u2;
	       v[2] = Map2Color4v1;
	       v[3] = Map2Color4v2;
	       break;
	    case GL_MAP2_INDEX:
	       v[0] = Map2Indexu1;
	       v[1] = Map2Indexu2;
	       v[2] = Map2Indexv1;
	       v[3] = Map2Indexv2;
	       break;
	    case GL_MAP2_NORMAL:
	       v[0] = Map2Normalu1;
	       v[1] = Map2Normalu2;
	       v[2] = Map2Normalv1;
	       v[3] = Map2Normalv2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       v[0] = Map2Texture1u1;
	       v[1] = Map2Texture1u2;
	       v[2] = Map2Texture1v1;
	       v[3] = Map2Texture1v2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       v[0] = Map2Texture2u1;
	       v[1] = Map2Texture2u2;
	       v[2] = Map2Texture2v1;
	       v[3] = Map2Texture2v2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       v[0] = Map2Texture3u1;
	       v[1] = Map2Texture3u2;
	       v[2] = Map2Texture3v1;
	       v[3] = Map2Texture3v2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       v[0] = Map2Texture4u1;
	       v[1] = Map2Texture4u2;
	       v[2] = Map2Texture4v1;
	       v[3] = Map2Texture4v2;
	       break;
	    case GL_MAP2_VERTEX_3:
	       v[0] = Map2Vertex3u1;
	       v[1] = Map2Vertex3u2;
	       v[2] = Map2Vertex3v1;
	       v[3] = Map2Vertex3v2;
	       break;
	    case GL_MAP2_VERTEX_4:
	       v[0] = Map2Vertex4u1;
	       v[1] = Map2Vertex4u2;
	       v[2] = Map2Vertex4v1;
	       v[3] = Map2Vertex4v2;
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapdv(target)" );
	 }
         break;
      default:
         gl_error( GL_INVALID_ENUM, "glGetMapdv(query)" );
   }
}


void glGetMapfv( GLenum target, GLenum query, GLfloat *v )
{
   GLuint i, n;
   GLfloat *data;

   switch (query) {
      case GL_COEFF:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       data = Map1Color4;
	       n = Map1Color4order * 4;
	       break;
	    case GL_MAP1_INDEX:
	       data = Map1Index;
	       n = Map1Indexorder;
	       break;
	    case GL_MAP1_NORMAL:
	       data = Map1Normal;
	       n = Map1Normalorder * 3;
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       data = Map1Texture1;
	       n = Map1Texture1order * 1;
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       data = Map1Texture2;
	       n = Map1Texture2order * 2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       data = Map1Texture3;
	       n = Map1Texture3order * 3;
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       data = Map1Texture4;
	       n = Map1Texture4order * 4;
	       break;
	    case GL_MAP1_VERTEX_3:
	       data = Map1Vertex3;
	       n = Map1Vertex3order * 3;
	       break;
	    case GL_MAP1_VERTEX_4:
	       data = Map1Vertex4;
	       n = Map1Vertex4order * 4;
	       break;
	    case GL_MAP2_COLOR_4:
	       data = Map2Color4;
	       n = Map2Color4uorder * Map2Color4vorder * 4;
	       break;
	    case GL_MAP2_INDEX:
	       data = Map2Index;
	       n = Map2Indexuorder * Map2Indexvorder;
	       break;
	    case GL_MAP2_NORMAL:
	       data = Map2Normal;
	       n = Map2Normaluorder * Map2Normalvorder * 3;
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       data = Map2Texture1;
	       n = Map2Texture1uorder * Map2Texture1vorder * 1;
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       data = Map2Texture2;
	       n = Map2Texture2uorder * Map2Texture2vorder * 2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       data = Map2Texture3;
	       n = Map2Texture3uorder * Map2Texture3vorder * 3;
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       data = Map2Texture4;
	       n = Map2Texture4uorder * Map2Texture4vorder * 4;
	       break;
	    case GL_MAP2_VERTEX_3:
	       data = Map2Vertex3;
	       n = Map2Vertex3uorder * Map2Vertex3vorder * 3;
	       break;
	    case GL_MAP2_VERTEX_4:
	       data = Map2Vertex4;
	       n = Map2Vertex4uorder * Map2Vertex4vorder * 4;
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapfv(target)" );
	 }
	 if (data) {
	    for (i=0;i<n;i++) {
	       v[i] = data[i];
	    }
	 }
         break;
      case GL_ORDER:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       *v = Map1Color4order;
	       break;
	    case GL_MAP1_INDEX:
	       *v = Map1Indexorder;
	       break;
	    case GL_MAP1_NORMAL:
	       *v = Map1Normalorder;
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       *v = Map1Texture1order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       *v = Map1Texture2order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       *v = Map1Texture3order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       *v = Map1Texture4order;
	       break;
	    case GL_MAP1_VERTEX_3:
	       *v = Map1Vertex3order;
	       break;
	    case GL_MAP1_VERTEX_4:
	       *v = Map1Vertex4order;
	       break;
	    case GL_MAP2_COLOR_4:
	       v[0] = Map2Color4uorder;
	       v[1] = Map2Color4vorder;
	       break;
	    case GL_MAP2_INDEX:
	       v[0] = Map2Indexuorder;
	       v[1] = Map2Indexvorder;
	       break;
	    case GL_MAP2_NORMAL:
	       v[0] = Map2Normaluorder;
	       v[1] = Map2Normalvorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       v[0] = Map2Texture1uorder;
	       v[1] = Map2Texture1vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       v[0] = Map2Texture2uorder;
	       v[1] = Map2Texture2vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       v[0] = Map2Texture3uorder;
	       v[1] = Map2Texture3vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       v[0] = Map2Texture4uorder;
	       v[1] = Map2Texture4vorder;
	       break;
	    case GL_MAP2_VERTEX_3:
	       v[0] = Map2Vertex3uorder;
	       v[1] = Map2Vertex3vorder;
	       break;
	    case GL_MAP2_VERTEX_4:
	       v[0] = Map2Vertex4uorder;
	       v[1] = Map2Vertex4vorder;
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapfv(target)" );
	 }
         break;
      case GL_DOMAIN:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       v[0] = Map1Color4u1;
	       v[1] = Map1Color4u2;
	       break;
	    case GL_MAP1_INDEX:
	       v[0] = Map1Indexu1;
	       v[1] = Map1Indexu2;
	       break;
	    case GL_MAP1_NORMAL:
	       v[0] = Map1Normalu1;
	       v[1] = Map1Normalu2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       v[0] = Map1Texture1u1;
	       v[1] = Map1Texture1u2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       v[0] = Map1Texture2u1;
	       v[1] = Map1Texture2u2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       v[0] = Map1Texture3u1;
	       v[1] = Map1Texture3u2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       v[0] = Map1Texture4u1;
	       v[1] = Map1Texture4u2;
	       break;
	    case GL_MAP1_VERTEX_3:
	       v[0] = Map1Vertex3u1;
	       v[1] = Map1Vertex3u2;
	       break;
	    case GL_MAP1_VERTEX_4:
	       v[0] = Map1Vertex4u1;
	       v[1] = Map1Vertex4u2;
	       break;
	    case GL_MAP2_COLOR_4:
	       v[0] = Map2Color4u1;
	       v[1] = Map2Color4u2;
	       v[2] = Map2Color4v1;
	       v[3] = Map2Color4v2;
	       break;
	    case GL_MAP2_INDEX:
	       v[0] = Map2Indexu1;
	       v[1] = Map2Indexu2;
	       v[2] = Map2Indexv1;
	       v[3] = Map2Indexv2;
	       break;
	    case GL_MAP2_NORMAL:
	       v[0] = Map2Normalu1;
	       v[1] = Map2Normalu2;
	       v[2] = Map2Normalv1;
	       v[3] = Map2Normalv2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       v[0] = Map2Texture1u1;
	       v[1] = Map2Texture1u2;
	       v[2] = Map2Texture1v1;
	       v[3] = Map2Texture1v2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       v[0] = Map2Texture2u1;
	       v[1] = Map2Texture2u2;
	       v[2] = Map2Texture2v1;
	       v[3] = Map2Texture2v2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       v[0] = Map2Texture3u1;
	       v[1] = Map2Texture3u2;
	       v[2] = Map2Texture3v1;
	       v[3] = Map2Texture3v2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       v[0] = Map2Texture4u1;
	       v[1] = Map2Texture4u2;
	       v[2] = Map2Texture4v1;
	       v[3] = Map2Texture4v2;
	       break;
	    case GL_MAP2_VERTEX_3:
	       v[0] = Map2Vertex3u1;
	       v[1] = Map2Vertex3u2;
	       v[2] = Map2Vertex3v1;
	       v[3] = Map2Vertex3v2;
	       break;
	    case GL_MAP2_VERTEX_4:
	       v[0] = Map2Vertex4u1;
	       v[1] = Map2Vertex4u2;
	       v[2] = Map2Vertex4v1;
	       v[3] = Map2Vertex4v2;
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapfv(target)" );
	 }
         break;
      default:
         gl_error( GL_INVALID_ENUM, "glGetMapfv(query)" );
   }
}


void glGetMapiv( GLenum target, GLenum query, GLint *v )
{
   GLuint i, n;
   GLfloat *data;

   switch (query) {
      case GL_COEFF:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       data = Map1Color4;
	       n = Map1Color4order * 4;
	       break;
	    case GL_MAP1_INDEX:
	       data = Map1Index;
	       n = Map1Indexorder;
	       break;
	    case GL_MAP1_NORMAL:
	       data = Map1Normal;
	       n = Map1Normalorder * 3;
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       data = Map1Texture1;
	       n = Map1Texture1order * 1;
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       data = Map1Texture2;
	       n = Map1Texture2order * 2;
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       data = Map1Texture3;
	       n = Map1Texture3order * 3;
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       data = Map1Texture4;
	       n = Map1Texture4order * 4;
	       break;
	    case GL_MAP1_VERTEX_3:
	       data = Map1Vertex3;
	       n = Map1Vertex3order * 3;
	       break;
	    case GL_MAP1_VERTEX_4:
	       data = Map1Vertex4;
	       n = Map1Vertex4order * 4;
	       break;
	    case GL_MAP2_COLOR_4:
	       data = Map2Color4;
	       n = Map2Color4uorder * Map2Color4vorder * 4;
	       break;
	    case GL_MAP2_INDEX:
	       data = Map2Index;
	       n = Map2Indexuorder * Map2Indexvorder;
	       break;
	    case GL_MAP2_NORMAL:
	       data = Map2Normal;
	       n = Map2Normaluorder * Map2Normalvorder * 3;
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       data = Map2Texture1;
	       n = Map2Texture1uorder * Map2Texture1vorder * 1;
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       data = Map2Texture2;
	       n = Map2Texture2uorder * Map2Texture2vorder * 2;
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       data = Map2Texture3;
	       n = Map2Texture3uorder * Map2Texture3vorder * 3;
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       data = Map2Texture4;
	       n = Map2Texture4uorder * Map2Texture4vorder * 4;
	       break;
	    case GL_MAP2_VERTEX_3:
	       data = Map2Vertex3;
	       n = Map2Vertex3uorder * Map2Vertex3vorder * 3;
	       break;
	    case GL_MAP2_VERTEX_4:
	       data = Map2Vertex4;
	       n = Map2Vertex4uorder * Map2Vertex4vorder * 4;
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapiv(target)" );
	 }
	 if (data) {
	    for (i=0;i<n;i++) {
	       v[i] = ROUNDF(data[i]);
	    }
	 }
         break;
      case GL_ORDER:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       *v = Map1Color4order;
	       break;
	    case GL_MAP1_INDEX:
	       *v = Map1Indexorder;
	       break;
	    case GL_MAP1_NORMAL:
	       *v = Map1Normalorder;
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       *v = Map1Texture1order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       *v = Map1Texture2order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       *v = Map1Texture3order;
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       *v = Map1Texture4order;
	       break;
	    case GL_MAP1_VERTEX_3:
	       *v = Map1Vertex3order;
	       break;
	    case GL_MAP1_VERTEX_4:
	       *v = Map1Vertex4order;
	       break;
	    case GL_MAP2_COLOR_4:
	       v[0] = Map2Color4uorder;
	       v[1] = Map2Color4vorder;
	       break;
	    case GL_MAP2_INDEX:
	       v[0] = Map2Indexuorder;
	       v[1] = Map2Indexvorder;
	       break;
	    case GL_MAP2_NORMAL:
	       v[0] = Map2Normaluorder;
	       v[1] = Map2Normalvorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       v[0] = Map2Texture1uorder;
	       v[1] = Map2Texture1vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       v[0] = Map2Texture2uorder;
	       v[1] = Map2Texture2vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       v[0] = Map2Texture3uorder;
	       v[1] = Map2Texture3vorder;
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       v[0] = Map2Texture4uorder;
	       v[1] = Map2Texture4vorder;
	       break;
	    case GL_MAP2_VERTEX_3:
	       v[0] = Map2Vertex3uorder;
	       v[1] = Map2Vertex3vorder;
	       break;
	    case GL_MAP2_VERTEX_4:
	       v[0] = Map2Vertex4uorder;
	       v[1] = Map2Vertex4vorder;
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapiv(target)" );
	 }
         break;
      case GL_DOMAIN:
	 switch (target) {
	    case GL_MAP1_COLOR_4:
	       v[0] = ROUNDF(Map1Color4u1);
	       v[1] = ROUNDF(Map1Color4u2);
	       break;
	    case GL_MAP1_INDEX:
	       v[0] = ROUNDF(Map1Indexu1);
	       v[1] = ROUNDF(Map1Indexu2);
	       break;
	    case GL_MAP1_NORMAL:
	       v[0] = ROUNDF(Map1Normalu1);
	       v[1] = ROUNDF(Map1Normalu2);
	       break;
	    case GL_MAP1_TEXTURE_COORD_1:
	       v[0] = ROUNDF(Map1Texture1u1);
	       v[1] = ROUNDF(Map1Texture1u2);
	       break;
	    case GL_MAP1_TEXTURE_COORD_2:
	       v[0] = ROUNDF(Map1Texture2u1);
	       v[1] = ROUNDF(Map1Texture2u2);
	       break;
	    case GL_MAP1_TEXTURE_COORD_3:
	       v[0] = ROUNDF(Map1Texture3u1);
	       v[1] = ROUNDF(Map1Texture3u2);
	       break;
	    case GL_MAP1_TEXTURE_COORD_4:
	       v[0] = ROUNDF(Map1Texture4u1);
	       v[1] = ROUNDF(Map1Texture4u2);
	       break;
	    case GL_MAP1_VERTEX_3:
	       v[0] = ROUNDF(Map1Vertex3u1);
	       v[1] = ROUNDF(Map1Vertex3u2);
	       break;
	    case GL_MAP1_VERTEX_4:
	       v[0] = ROUNDF(Map1Vertex4u1);
	       v[1] = ROUNDF(Map1Vertex4u2);
	       break;
	    case GL_MAP2_COLOR_4:
	       v[0] = ROUNDF(Map2Color4u1);
	       v[1] = ROUNDF(Map2Color4u2);
	       v[2] = ROUNDF(Map2Color4v1);
	       v[3] = ROUNDF(Map2Color4v2);
	       break;
	    case GL_MAP2_INDEX:
	       v[0] = ROUNDF(Map2Indexu1);
	       v[1] = ROUNDF(Map2Indexu2);
	       v[2] = ROUNDF(Map2Indexv1);
	       v[3] = ROUNDF(Map2Indexv2);
	       break;
	    case GL_MAP2_NORMAL:
	       v[0] = ROUNDF(Map2Normalu1);
	       v[1] = ROUNDF(Map2Normalu2);
	       v[2] = ROUNDF(Map2Normalv1);
	       v[3] = ROUNDF(Map2Normalv2);
	       break;
	    case GL_MAP2_TEXTURE_COORD_1:
	       v[0] = ROUNDF(Map2Texture1u1);
	       v[1] = ROUNDF(Map2Texture1u2);
	       v[2] = ROUNDF(Map2Texture1v1);
	       v[3] = ROUNDF(Map2Texture1v2);
	       break;
	    case GL_MAP2_TEXTURE_COORD_2:
	       v[0] = ROUNDF(Map2Texture2u1);
	       v[1] = ROUNDF(Map2Texture2u2);
	       v[2] = ROUNDF(Map2Texture2v1);
	       v[3] = ROUNDF(Map2Texture2v2);
	       break;
	    case GL_MAP2_TEXTURE_COORD_3:
	       v[0] = ROUNDF(Map2Texture3u1);
	       v[1] = ROUNDF(Map2Texture3u2);
	       v[2] = ROUNDF(Map2Texture3v1);
	       v[3] = ROUNDF(Map2Texture3v2);
	       break;
	    case GL_MAP2_TEXTURE_COORD_4:
	       v[0] = ROUNDF(Map2Texture4u1);
	       v[1] = ROUNDF(Map2Texture4u2);
	       v[2] = ROUNDF(Map2Texture4v1);
	       v[3] = ROUNDF(Map2Texture4v2);
	       break;
	    case GL_MAP2_VERTEX_3:
	       v[0] = ROUNDF(Map2Vertex3u1);
	       v[1] = ROUNDF(Map2Vertex3u2);
	       v[2] = ROUNDF(Map2Vertex3v1);
	       v[3] = ROUNDF(Map2Vertex3v2);
	       break;
	    case GL_MAP2_VERTEX_4:
	       v[0] = ROUNDF(Map2Vertex4u1);
	       v[1] = ROUNDF(Map2Vertex4u2);
	       v[2] = ROUNDF(Map2Vertex4v1);
	       v[3] = ROUNDF(Map2Vertex4v2);
	       break;
	    default:
	       gl_error( GL_INVALID_ENUM, "glGetMapiv(target)" );
	 }
         break;
      default:
         gl_error( GL_INVALID_ENUM, "glGetMapiv(query)" );
   }
}



void gl_evalcoord1(GLfloat u)
{
  GLfloat vertex[4];
  GLfloat normal[3];
  GLfloat fcolor[4];
  GLint icolor[4];
  GLint *colorptr;
  GLfloat texcoord[4];
  GLuint index;
  register GLfloat uu;

  /** Vertex **/
  if (CC.Eval.Map1Vertex4) 
  {
    uu = (u-Map1Vertex4u1) / (Map1Vertex4u2-Map1Vertex4u1);
    horner_bezier_curve(Map1Vertex4, vertex, uu, 4, Map1Vertex4order);
  }
  else if (CC.Eval.Map1Vertex3) 
  {
    uu = (u-Map1Vertex3u1) / (Map1Vertex3u2-Map1Vertex3u1);
    horner_bezier_curve(Map1Vertex3, vertex, uu, 3, Map1Vertex3order);

    vertex[3] = 1.0;
  }

  /** Color Index **/
  if (CC.Eval.Map1Index) 
  {
    GLfloat findex;
    uu = (u-Map1Indexu1) / (Map1Indexu2-Map1Indexu1);
    horner_bezier_curve(Map1Index, &findex, uu, 1, Map1Indexorder);
    index = (GLuint) (GLint) findex;
  }
  else 
    index = CC.Current.Index;

  /** Color **/
  if (CC.Eval.Map1Color4) 
  {
    uu = (u-Map1Color4u1) / (Map1Color4u2-Map1Color4u1);
    horner_bezier_curve(Map1Color4, fcolor, uu, 4, Map1Color4order);
    icolor[0] = (GLint) (fcolor[0] * CC.RedScale);
    icolor[1] = (GLint) (fcolor[1] * CC.GreenScale);
    icolor[2] = (GLint) (fcolor[2] * CC.BlueScale);
    icolor[3] = (GLint) (fcolor[3] * CC.AlphaScale);
    colorptr = icolor;
  }
  else 
  {
    colorptr = CC.Current.IntColor;
  }

  /** Normal Vector **/
  if (CC.Eval.Map1Normal) 
  {
    uu = (u-Map1Normalu1) / (Map1Normalu2-Map1Normalu1);
    horner_bezier_curve(Map1Normal, normal, uu, 3, Map1Normalorder);
  }
  else 
  {
    normal[0] = CC.Current.Normal[0];
    normal[1] = CC.Current.Normal[1];
    normal[2] = CC.Current.Normal[2];
  }

  /** Texture Coordinates **/
  if (CC.Eval.Map1TextureCoord4) 
  {
    uu = (u-Map1Texture4u1) / (Map1Texture4u2-Map1Texture4u1);
    horner_bezier_curve(Map1Texture4, texcoord, uu, 4, Map1Texture4order);
  }
  else if (CC.Eval.Map1TextureCoord3) 
  {
    uu = (u-Map1Texture3u1) / (Map1Texture3u2-Map1Texture3u1);
    horner_bezier_curve(Map1Texture3, texcoord, uu, 3, Map1Texture3order);
    texcoord[3] = 1.0;
  }
  else if (CC.Eval.Map1TextureCoord2) 
  {
    uu = (u-Map1Texture2u1) / (Map1Texture2u2-Map1Texture2u1);
    horner_bezier_curve(Map1Texture2, texcoord, uu, 2, Map1Texture2order);
    
    texcoord[2] = 0.0;
    texcoord[3] = 1.0;
  }
  else if (CC.Eval.Map1TextureCoord1) 
  {
    uu = (u-Map1Texture1u1) / (Map1Texture1u2-Map1Texture1u1);
    horner_bezier_curve(Map1Texture1, texcoord, uu, 1, Map1Texture1order);
    
    texcoord[1] = 0.0;
    texcoord[2] = 0.0;
    texcoord[3] = 1.0;
  }
  else 
  {
    texcoord[0] = CC.Current.TexCoord[0];
    texcoord[1] = CC.Current.TexCoord[1];
    texcoord[2] = CC.Current.TexCoord[2];
    texcoord[3] = CC.Current.TexCoord[3];
  }
  
  gl_eval_vertex( vertex, normal, colorptr, index, texcoord );
}

void glEvalCoord1f( GLfloat u )
{
   if (CC.CompileFlag) {
      gl_save_evalcoord1( u );
   }
   if (CC.ExecuteFlag) {
      gl_evalcoord1( u );
   }
}


void glEvalCoord1fv( const GLfloat *u )
{
   if (CC.CompileFlag) {
      gl_save_evalcoord1( *u );
   }
   if (CC.ExecuteFlag) {
      gl_evalcoord1( *u );
   }
}


void glEvalCoord1d( GLdouble u )
{
   if (CC.CompileFlag) {
      gl_save_evalcoord1( (GLfloat) u );
   }
   if (CC.ExecuteFlag) {
      gl_evalcoord1( (GLfloat) u );
   }
}


void glEvalCoord1dv( const GLdouble *u )
{
   if (CC.CompileFlag) {
      gl_save_evalcoord1( (GLfloat) *u );
   }
   if (CC.ExecuteFlag) {
      gl_evalcoord1( (GLfloat) *u );
   }
}


void gl_evalcoord2( GLfloat u, GLfloat v )
{
   GLfloat vertex[4];
   GLfloat normal[3];
   GLfloat fcolor[4];
   GLint icolor[4];
   GLint *colorptr;
   GLfloat texcoord[4];
   GLuint index;
   register GLfloat uu, vv;

#define CROSS_PROD(n, u, v) \
  (n)[0] = (u)[1]*(v)[2] - (u)[2]*(v)[1]; \
  (n)[1] = (u)[2]*(v)[0] - (u)[0]*(v)[2]; \
  (n)[2] = (u)[0]*(v)[1] - (u)[1]*(v)[0]
#define NORMALIZE(n) \
  { GLfloat l = sqrt((n)[0]*(n)[0] + (n)[1]*n[1] + (n)[2]*(n)[2]); \
    if(l > 0.000001) { (n)[0]/=l; (n)[1]/=l; (n)[2]/=l; } }

   /** Vertex **/
   if(CC.Eval.Map2Vertex4) 
   {
     uu = (u-Map2Vertex4u1) / (Map2Vertex4u2-Map2Vertex4u1);
     vv = (v-Map2Vertex4v1) / (Map2Vertex4v2-Map2Vertex4v1);

     if (CC.Eval.AutoNormal)
     {
       GLfloat du[4], dv[4];

       de_casteljau_surf(Map2Vertex4, vertex, du, dv, uu, vv, 4,
			 Map2Vertex4uorder, Map2Vertex4vorder);

       CROSS_PROD(normal, du, dv);
       NORMALIZE(normal);
     }
     else
       horner_bezier_surf(Map2Vertex4, vertex, uu, vv, 4,
			  Map2Vertex4uorder, Map2Vertex4vorder);
   }
   else if (CC.Eval.Map2Vertex3) 
   {
     uu = (u-Map2Vertex3u1) / (Map2Vertex3u2-Map2Vertex3u1);
     vv = (v-Map2Vertex3v1) / (Map2Vertex3v2-Map2Vertex3v1);

     if (CC.Eval.AutoNormal)
     {
       GLfloat du[3], dv[3];
       de_casteljau_surf(Map2Vertex3, vertex, du, dv, uu, vv, 3,
			 Map2Vertex3uorder, Map2Vertex3vorder);

       CROSS_PROD(normal, du, dv);
       NORMALIZE(normal);
     }
     else
       horner_bezier_surf(Map2Vertex3, vertex, uu, vv, 3,
			  Map2Vertex3uorder, Map2Vertex3vorder);

     vertex[3] = 1.0;
   }
#undef NORMALIZE
#undef CROSS_PROD
   
   /** Color Index **/
   if (CC.Eval.Map2Index) 
   {
     GLfloat findex;
     uu = (u-Map2Indexu1) / (Map2Indexu2-Map2Indexu1);
     vv = (v-Map2Indexv1) / (Map2Indexv2-Map2Indexv1);

     horner_bezier_surf(Map2Index, &findex, uu, vv, 1,
			Map2Indexuorder, Map2Indexvorder);
     index = (GLuint) (GLint) findex;
   }
   else 
     index = CC.Current.Index;

   /** Color **/
   if (CC.Eval.Map2Color4) 
   {
     uu = (u-Map2Color4u1) / (Map2Color4u2-Map2Color4u1);
     vv = (v-Map2Color4v1) / (Map2Color4v2-Map2Color4v1);

     horner_bezier_surf(Map2Color4, fcolor, uu, vv, 4,
			Map2Color4uorder, Map2Color4vorder);
     icolor[0] = (GLint) (fcolor[0] * CC.RedScale);
     icolor[1] = (GLint) (fcolor[1] * CC.GreenScale);
     icolor[2] = (GLint) (fcolor[2] * CC.BlueScale);
     icolor[3] = (GLint) (fcolor[3] * CC.AlphaScale);
     colorptr = icolor;
   }
   else 
   {
      colorptr = CC.Current.IntColor;
   }

   /** Normal **/
   if(!CC.Eval.AutoNormal || (!CC.Eval.Map2Vertex3 && !CC.Eval.Map2Vertex4))
   {
     if (CC.Eval.Map2Normal) 
     {
       uu = (u-Map2Normalu1) / (Map2Normalu2-Map2Normalu1);
       vv = (v-Map2Normalv1) / (Map2Normalv2-Map2Normalv1);

       horner_bezier_surf(Map2Normal, normal, uu, vv, 3,
			  Map2Normaluorder, Map2Normalvorder);
     }
     else 
     {
       normal[0] = CC.Current.Normal[0];
       normal[1] = CC.Current.Normal[1];
       normal[2] = CC.Current.Normal[2];
     }
   }

   /** Texture Coordinates **/
   if (CC.Eval.Map2TextureCoord4) 
   {
     uu = (u-Map2Texture4u1) / (Map2Texture4u2-Map2Texture4u1);
     vv = (v-Map2Texture4v1) / (Map2Texture4v2-Map2Texture4v1);

     horner_bezier_surf(Map2Texture4, texcoord, uu, vv, 4,
			Map2Texture4uorder, Map2Texture4vorder);
   }
   else if (CC.Eval.Map2TextureCoord3) 
   {
     uu = (u-Map2Texture3u1) / (Map2Texture3u2-Map2Texture3u1);
     vv = (v-Map2Texture3v1) / (Map2Texture3v2-Map2Texture3v1);

     horner_bezier_surf(Map2Texture3, texcoord, uu, vv, 3,
			Map2Texture3uorder, Map2Texture3vorder);

     texcoord[3] = 1.0;
   }
   else if (CC.Eval.Map2TextureCoord2) 
   {
     uu = (u-Map2Texture2u1) / (Map2Texture2u2-Map2Texture2u1);
     vv = (v-Map2Texture2v1) / (Map2Texture2v2-Map2Texture2v1);

     horner_bezier_surf(Map2Texture2, texcoord, uu, vv, 2,
			Map2Texture2uorder, Map2Texture2vorder);

     texcoord[2] = 0.0;
     texcoord[3] = 1.0;
   }
   else if (CC.Eval.Map2TextureCoord1) 
   {
     uu = (u-Map2Texture1u1) / (Map2Texture1u2-Map2Texture1u1);
     vv = (v-Map2Texture1v1) / (Map2Texture1v2-Map2Texture1v1);

     horner_bezier_surf(Map2Texture1, texcoord, uu, vv, 1,
			Map2Texture1uorder, Map2Texture1vorder);

     texcoord[1] = 0.0;
     texcoord[2] = 0.0;
     texcoord[3] = 1.0;
   }
   else 
   {
     texcoord[0] = CC.Current.TexCoord[0];
     texcoord[1] = CC.Current.TexCoord[1];
     texcoord[2] = CC.Current.TexCoord[2];
     texcoord[3] = CC.Current.TexCoord[3];
   }

   gl_eval_vertex( vertex, normal, colorptr, index, texcoord );
}


void glEvalCoord2f( GLfloat u, GLfloat v )
{
   if (CC.CompileFlag) {
      gl_save_evalcoord2( u, v );
   }
   if (CC.ExecuteFlag) {
      gl_evalcoord2( u, v );
   }
}


void glEvalCoord2fv( const GLfloat *u )
{
   if (CC.CompileFlag) {
      gl_save_evalcoord2( u[0], u[1] );
   }
   if (CC.ExecuteFlag) {
      gl_evalcoord2( u[0], u[1] );
   }
}


void glEvalCoord2d( GLdouble u, GLdouble v )
{
   if (CC.CompileFlag) {
      gl_save_evalcoord2( (GLfloat) u, (GLfloat) v );
   }
   if (CC.ExecuteFlag) {
      gl_evalcoord2( (GLfloat) u, (GLfloat) v );
   }
}


void glEvalCoord2dv( const GLdouble *u )
{
   if (CC.CompileFlag) {
      gl_save_evalcoord2( (GLfloat) u[0], (GLfloat) u[1] );
   }
   if (CC.ExecuteFlag) {
      gl_evalcoord2( (GLfloat) u[0], (GLfloat) u[1] );
   }
}



void gl_mapgrid1( GLint un, GLfloat u1, GLfloat u2 )
{
   if (INSIDE_BEGIN_END) {
      gl_error( GL_INVALID_OPERATION, "glMapGrid1f" );
      return;
   }
   if (un<1) {
      gl_error( GL_INVALID_VALUE, "glMapGrid1f" );
      return;
   }
   CC.Eval.MapGrid1un = un;
   CC.Eval.MapGrid1u1 = u1;
   CC.Eval.MapGrid1u2 = u2;
}


void glMapGrid1f( GLint un, GLfloat u1, GLfloat u2 )
{
   if (CC.CompileFlag) {
      gl_save_mapgrid1( un, u1, u2 );
   }
   if (CC.ExecuteFlag) {
      gl_mapgrid1( un, u1, u2 );
   }
}


void glMapGrid1d( GLint un, GLdouble u1, GLdouble u2 )
{
   if (CC.CompileFlag) {
      gl_save_mapgrid1( un, (GLfloat) u1, (GLfloat) u2 );
   }
   if (CC.ExecuteFlag) {
      gl_mapgrid1( un, (GLfloat) u1, (GLfloat) u2 );
   }
}



void gl_mapgrid2( GLint un, GLfloat u1, GLfloat u2,
		  GLint vn, GLfloat v1, GLfloat v2 )
{
   if (INSIDE_BEGIN_END) {
      gl_error( GL_INVALID_OPERATION, "glMapGrid2f" );
      return;
   }
   if (un<1) {
      gl_error( GL_INVALID_VALUE, "glMapGrid2f(un)" );
      return;
   }
   if (vn<1) {
      gl_error( GL_INVALID_VALUE, "glMapGrid2f(vn)" );
      return;
   }
   CC.Eval.MapGrid2un = un;
   CC.Eval.MapGrid2u1 = u1;
   CC.Eval.MapGrid2u2 = u2;
   CC.Eval.MapGrid2vn = vn;
   CC.Eval.MapGrid2v1 = v1;
   CC.Eval.MapGrid2v2 = v2;
}


void glMapGrid2f( GLint un, GLfloat u1, GLfloat u2,
		  GLint vn, GLfloat v1, GLfloat v2 )
{
   if (CC.CompileFlag) {
      gl_save_mapgrid2( un, u1, u2, vn, v1, v2 );
   }
   if (CC.ExecuteFlag) {
      gl_mapgrid2( un, u1, u2, vn, v1, v2 );
   }
}


void glMapGrid2d( GLint un, GLdouble u1, GLdouble u2,
		  GLint vn, GLdouble v1, GLdouble v2 )
{
   if (CC.CompileFlag) {
      gl_save_mapgrid2( un, (GLfloat) u1, (GLfloat) u2,
		        vn, (GLfloat) v1, (GLfloat) v2 );
   }
   if (CC.ExecuteFlag) {
      gl_mapgrid2( un, (GLfloat) u1, (GLfloat) u2,
		   vn, (GLfloat) v1, (GLfloat) v2 );
   }
}



void glEvalPoint1( GLint i )
{
   if (CC.CompileFlag) {
      gl_save_evalpoint1( i );
   }
   if (CC.ExecuteFlag) {
      GLfloat u, du;

      if (i==0) {
	 u = CC.Eval.MapGrid1u1;
      }
      else if (i==CC.Eval.MapGrid1un) {
	 u = CC.Eval.MapGrid1u2;
      }
      else {
	 du = (CC.Eval.MapGrid1u2 - CC.Eval.MapGrid1u1)
	      / (GLfloat) CC.Eval.MapGrid1un;
	 u = i * du + CC.Eval.MapGrid1u1;
      }
      gl_evalcoord1( u );
   }
}



void glEvalPoint2( GLint i, GLint j )
{
   if (CC.CompileFlag) {
      gl_save_evalpoint2( i, j );
   }
   if (CC.ExecuteFlag) {
      GLfloat u, du;
      GLfloat v, dv;

      if (i==0) {
	 u = CC.Eval.MapGrid2u1;
      }
      else if (i==CC.Eval.MapGrid2un) {
	 u = CC.Eval.MapGrid2u2;
      }
      else {
	 du = (CC.Eval.MapGrid2u2 - CC.Eval.MapGrid2u1)
	       / (GLfloat) CC.Eval.MapGrid2un;
	 u = i * du + CC.Eval.MapGrid2u1;
      }

      if (j==0) {
	 v = CC.Eval.MapGrid2v1;
      }
      else if (j==CC.Eval.MapGrid2vn) {
	 v = CC.Eval.MapGrid2v2;
      }
      else {
	 dv = (CC.Eval.MapGrid2v2 - CC.Eval.MapGrid2v1)
	       / (GLfloat) CC.Eval.MapGrid2vn;
	 v = j * dv + CC.Eval.MapGrid2v1;
      }

      gl_evalcoord2( u, v );
   }
}



void glEvalMesh1( GLenum mode, GLint i1, GLint i2 )
{
   GLint i;
   GLfloat u, du;
   GLenum prim;

   if (CC.CompileFlag) {
      gl_save_evalmesh1( mode, i1, i2 );
   }
   if (CC.ExecuteFlag) {

      if (INSIDE_BEGIN_END) {
	 gl_error( GL_INVALID_OPERATION, "glEvalMesh1" );
	 return;
      }

      switch (mode) {
	 case GL_POINT:
	    prim = GL_POINTS;
	    break;
	 case GL_LINE:
	    prim = GL_LINE_STRIP;
	    break;
	 default:
	    gl_error( GL_INVALID_ENUM, "glEvalMesh1(mode)" );
	    return;
      }

      du = (CC.Eval.MapGrid1u2 - CC.Eval.MapGrid1u1)
	       / (GLfloat) CC.Eval.MapGrid1un;

      gl_begin( prim );
      for (i=i1;i<=i2;i++) {
	 if (i==0) {
	    u = CC.Eval.MapGrid1u1;
	 }
	 else if (i==CC.Eval.MapGrid1un) {
	    u = CC.Eval.MapGrid1u2;
	 }
	 else {
	    u = i * du + CC.Eval.MapGrid1u1;
	 }
	 gl_evalcoord1( u );
      }
      gl_end();
   }
}



void glEvalMesh2( GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2 )
{
   GLint i, j;
   GLfloat u, du, v, dv, v1, v2;

   if (CC.CompileFlag) {
      gl_save_evalmesh2( mode, i1, i2, j1, j2 );
   }
   if (CC.ExecuteFlag) {

      if (INSIDE_BEGIN_END) {
	 gl_error( GL_INVALID_OPERATION, "glEvalMesh2" );
	 return;
      }

      du = (CC.Eval.MapGrid2u2 - CC.Eval.MapGrid2u1)
	       / (GLfloat) CC.Eval.MapGrid2un;
      dv = (CC.Eval.MapGrid2v2 - CC.Eval.MapGrid2v1)
	       / (GLfloat) CC.Eval.MapGrid2vn;

#define I_TO_U( I, U )				\
	   if ((I)==0) {		       	\
	      U = CC.Eval.MapGrid2u1;		\
	   }					\
	   else if ((I)==CC.Eval.MapGrid2un) {	\
	      U = CC.Eval.MapGrid2u2;		\
	   }					\
	   else {				\
	      U = (I) * du + CC.Eval.MapGrid2u1;\
	   }

#define J_TO_V( J, V )				\
	   if ((J)==0) {			\
	      V = CC.Eval.MapGrid2v1;		\
	   }					\
	   else if ((J)==CC.Eval.MapGrid2vn) {	\
	      V = CC.Eval.MapGrid2v2;		\
	   }					\
	   else {				\
	      V = (J) * dv + CC.Eval.MapGrid2v1;\
	   }

      switch (mode) {
	 case GL_POINT:
	    gl_begin( GL_POINTS );
	    for (j=j1;j<=j2;j++) {
	       J_TO_V( j, v );
	       for (i=i1;i<=i2;i++) {
		  I_TO_U( i, u );
		  gl_evalcoord2( u, v );
	       }
	    }
	    gl_end();
	    break;
	 case GL_LINE:
	    for (j=j1;j<=j2;j++) {
	       J_TO_V( j, v );
	       gl_begin( GL_LINE_STRIP );
	       for (i=i1;i<=i2;i++) {
		  I_TO_U( i, u );
		  gl_evalcoord2( u, v );
	       }
	       gl_end();
	    }
	    for (i=i1;i<=i2;i++) {
	       I_TO_U( i, u );
	       gl_begin( GL_LINE_STRIP );
	       for (j=j1;j<=j2;j++) {
		  J_TO_V( j, v );
		  gl_evalcoord2( u, v );
	       }
	       gl_end();
	    }
	    break;
	 case GL_FILL:
	    for (j=j1;j<j2;j++) {
	       /* NOTE: a quad strip can't be used because the four */
	       /* can't be guaranteed to be coplanar! */
	       gl_begin( GL_TRIANGLE_STRIP );
	       J_TO_V( j, v1 );
	       J_TO_V( j+1, v2 );
	       for (i=i1;i<=i2;i++) {
		  I_TO_U( i, u );
		  gl_evalcoord2( u, v1 );
		  gl_evalcoord2( u, v2 );
	       }
	       gl_end();
	    }
	    break;
	 default:
	    gl_error( GL_INVALID_ENUM, "glEvalMesh2(mode)" );
	    return;
      }

#undef I_TO_U
#undef J_TO_V
   }
}

