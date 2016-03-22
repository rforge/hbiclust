#ifndef __R_UTILS__
#define __R_UTILS__

/*******************************************************************************
 * @file    RUtils.h
 * @author  Alexandre Arsenault <alx.arsenault@gmail.com>
 * @brief   Utility classes for point, size and color.
 * @date    10/07/2015
 ******************************************************************************/

#include <iostream>
#include <vector>

namespace R
{
    /**************************************************************************
    * R::Size2D_T.
    **************************************************************************/
	template<typename T>
	struct Size2D_T
	{
		Size2D_T(const T& nrow = 0, const T& ncol = 0):
		row(nrow), col(ncol)
		{

		}

		T row, col;
	};

    /**************************************************************************
    * R::Size2D.
    **************************************************************************/
	typedef Size2D_T<unsigned int> Size2D;
    
    /**************************************************************************
    * R::Point2D_T.
    **************************************************************************/
    template<typename T>
    struct Point2D_T
    {
        Point2D_T()
        {
            
        }
        
        Point2D_T(const T& X, const T& Y):
        x(X), y(Y)
        {
            
        }
        
        T x, y;
    };
    
    /**************************************************************************
    * R::Point.
    **************************************************************************/
    typedef Point2D_T<double> Point;
    
    /**************************************************************************
    * R::Line_T.
    **************************************************************************/
    template<typename T>
    struct Line_T
    {
        Line_T(const Point2D_T<T>& p1, const Point2D_T<T>& p2):
        pt1(p1), pt2(p2)
        {
            
        }
        
        Point2D_T<T> pt1, pt2;
    };
    
    /**************************************************************************
    * R::Line.
    **************************************************************************/
    typedef Line_T<double> Line;
    
    /**************************************************************************
    * R::Color_T.
    **************************************************************************/
    template<typename T>
    struct Color_T
    {
        Color_T()
        {
            
        }
        
        Color_T(const T& R,
                const T& G,
                const T& B,
                const T& A):
        r(R), g(G), b(B), a(A)
        {
            
        }
        
        T r, g, b, a;
    };
    
    /**************************************************************************
    * R::Color.
    **************************************************************************/
    typedef Color_T<double> Color;

    /**************************************************************************
    * R::ColorVector.
    **************************************************************************/
    typedef std::vector<Color> ColorVector;
    
    R::ColorVector GetHeatColors(const int& n);
    R::ColorVector GetRainbowColors(const int& n);
}

#endif // __R_UTILS__