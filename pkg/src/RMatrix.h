#ifndef __R_MATRIX__
#define __R_MATRIX__

/*******************************************************************************
 * @file    RMatrix.h
 * @author  Alexandre Arsenault <alx.arsenault@gmail.com>
 * @brief   Utility classes to manipule data from R to cpp without owning 
 *          the data poin.
 * @date    10/07/2015
 ******************************************************************************/

#include "RUtils.h"

#include <vector>
#include <cassert>
#include <cmath>
#include <iostream>

namespace R
{
    /**************************************************************************
    * R::Matrix2D.
    **************************************************************************/
	template <typename T>
	class Matrix2D
	{
	public:
		Matrix2D(T* data, const Size2D& size):
		_data(data),
		_size(size)
		{

		}

		inline T& operator () (const unsigned int& row, 
                               const unsigned int& col)
		{
			unsigned int index = row + col * _size.row;
			return _data[index];
		}

		inline T& operator () (const unsigned int& index)
		{
			return _data[index];
		}
        
        inline const T& operator () (const unsigned int& row, 
                                     const unsigned int& col) const
        {
            unsigned int index = row + col * _size.row;
            return _data[index];
        }
        
        inline const T& operator () (const unsigned int& index) const
        {
            return _data[index];
        }

		T* GetData()
		{
			return _data;
		}
        
        Size2D size() const
        {
            return _size;
        }

	private:
		T* _data;
		Size2D _size;
	};
    

    /**************************************************************************
    * R::Vector.
    **************************************************************************/
    template <typename T>
    class Vector
    {
    public:
        Vector(T* data, const int& size):
        _data(data),
        _size(size)
        {
            
        }
        
        Vector(const Vector& vec):
        _data(vec._data),
        _size(vec._size)
        {
            
        }
        
        int size() const
        {
            return _size;
        }
        
        inline T& operator [] (const unsigned int& index)
        {
            return _data[index];
        }
        
        T* data()
        {
            return _data;
        }
        
        T* begin()
        {
            return _data;
        }
        
        T* end()
        {
            return &_data[_size];
        }
        
    private:
        T* _data;
        int _size;
    };
}

namespace ax
{
    namespace Math
    {
        /**********************************************************************
        * ax::Math::Vector.
        **********************************************************************/
        template<typename T>
        class Vector
        {
        public:
            Vector(const std::vector<T>& pt)
            {
                _vec = pt;
            }
            
            Vector(const Vector<T>& pt)
            {
                _vec = pt._vec;
            }
            
            
            Vector()
            {
            }

            Vector(const int& size)
            
            {
                _vec.resize(size);
            }
            
            Vector(const T& x, const T& y)
            {
                _vec.resize(2);
                _vec[0] = x;
                _vec[1] = y;
            }
            
            Vector(const T& x, const T& y, const T& z)
            {
                _vec.resize(3);
                _vec[0] = x;
                _vec[1] = y;
                _vec[2] = z;
            }
            
            T& operator [] (const int& index)
            {
                return _vec[index];
            }
            
            T* begin()
            {
                return &_vec[0];
            }
            
            T* end()
            {
                return (&_vec[_vec.size() - 1]) + 1;
            }
            
            Vector<T> operator + (const Vector<T>& vec)
            {
                Vector<T> answer;
                answer._vec = _vec;
                
                for(int i = 0; i < _vec.size(); i++)
                {
                    answer._vec[i] = _vec[i] + vec._vec[i];
                }
                
                return answer;
            }
            
            Vector<T> operator + (const T& value)
            {
                Vector<T> answer;
                answer._vec = _vec;
                
                for(int i = 0; i < _vec.size(); i++)
                {
                    answer._vec[i] += value;
                }
                
                return answer;
            }
            
            Vector<T> operator - (const Vector<T>& vec)
            {
                Vector<T> answer;
                answer._vec = _vec;
                
                for(int i = 0; i < _vec.size(); i++)
                {
                    answer._vec[i] = _vec[i] - vec._vec[i];
                }
                
                return answer;
            }
            
            Vector<T> operator * (const Vector<T>& vec)
            {
                Vector<T> answer;
                answer._vec = _vec;
                
                for(int i = 0; i < _vec.size(); i++)
                {
                    answer._vec[i] = _vec[i] * vec._vec[i];
                }
                
                return answer;
            }
            
            Vector<T> operator * (const T& value)
            {
                Vector<T> answer;
                answer._vec = _vec;
                
                for(int i = 0; i < _vec.size(); i++)
                {
                    answer._vec[i] *= value;
                }
                
                return answer;
            }
            
            Vector<T> operator / (const Vector<T>& vec)
            {
                Vector<T> answer;
                answer._vec = _vec;
                
                for(int i = 0; i < _vec.size(); i++)
                {
                    answer._vec[i] = _vec[i] / vec._vec[i];
                }
                
                return answer;
            }
            
            Vector<T> operator / (const T& value)
            {
                Vector<T> answer;
                answer._vec = _vec;
                
                for(int i = 0; i < _vec.size(); i++)
                {
                    answer._vec[i] /= value;
                }
                
                return answer;
            }
            
            inline int size() const
            {
                return _vec.size();
            }
            
            inline void resize(const int& size) const
            {
                _vec.resize(size);
            }
            
            T Dist(const Vector<T>& pt) const
            {
                // assert(_vec.size() == pt.size());
                
                double dist = 0.0;
                
                for(int i = 0; i < _vec.size(); i++)
                {
                    dist += pow(_vec[i] - pt._vec[i], 2.0);
                }
                
                return sqrt(dist);
            }
            
            std::vector<T> CopyVector() const
            {
                return _vec;
            }
            
            std::vector<T>& getStdVector()
            {
                return _vec;
            }
            
            void Print() const
            {
                std::cout << "(";
                for(int i = 0; i < _vec.size() - 1; i++)
                {
                    std::cout << _vec[i] << ", ";
                }
                std::cout << _vec[_vec.size()-1] << ")" << std::endl;
            }
            
        private:
            std::vector<T> _vec;
        };
    }
}

#endif // __R_MATRIX__
