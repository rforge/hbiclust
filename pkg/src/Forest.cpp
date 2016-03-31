//R CMD SHLIB biclus_3Aug.cpp
#include <stack>
#include <math.h>
#include <iostream>
#include <limits>
#define M(i, j) data[j*row+i]
#define T(i,j) data[i*col+j]
#define Mrg(i,j) merge[j*(row+col-2)+i]
//using namespace std;

// int row, col;
// double *Data;
// int *merge;
struct Link
{
	int L, R;	
};
/*
void PrintDis(double **Dist, int n, bool *val)
{
	for(int i=0; i<n; i++)
	{
		for(int j=0; j<n; j++)
			if(val[i] && val[j])
				cout<<Dist[i][j] << ' ';
		cout<<'\n';
	}
}*/
double EuclideanDist(double *data, int i, int j, int row, int col, bool T, bool *cv)
{
	double tempd=0;
	for(int k=0;k<col;k++)
	{
		if(cv[k])
		{
			double dif;
			if(T)
				dif = M(i,k)-M(j,k);
			else 
				dif = T(i,k)-T(j,k);
			tempd += pow(dif, 2);
		}
	}
	return sqrt(tempd);
}
void PDist(double *data, double **Dist, int row, int col, bool T, bool *rv, bool *cv)
{
	for(int i=0; i<row; i++)
	{
		if(rv[i])
		{
			Dist[i][i]=0;
			for(int j=i+1;j<row;j++)
			{
				if(rv[j])
				{
					Dist[i][j] = EuclideanDist(data, i, j, row, col, T, cv);
					Dist[j][i] = Dist[i][j];
				}
			}
		}
	}
}
double Min_Index(double **Dist, int row, int *ind, bool *v)
{
	double min = std::numeric_limits<double>::max();
	ind[0]=-1;
	ind[1]=-1;
	for(int i=0; i<row; i++)		
		for(int j=i+1;j<row;j++)
			if(Dist[i][j]<min && v[i] && v[j])
			{
				min = Dist[i][j];
				ind[0]=i;
				ind[1]=j;
			}
	return min;
}
void Extract(Link *l, int *order, int ind)
{
	std::stack<int> st;	
	st.push(l[ind].R);
	st.push(l[ind].L);

	int n=0;
	while(st.size()!=0)	
	{
		int cix=st.top();			
		st.pop();
		if(cix<0)
		{
			order[n]=-cix;
			n++;
		}
		else
		if(cix>0)
		{
			st.push(l[cix].R);
			st.push(l[cix].L);
		}		
	}
}

extern "C" {
void Agl(int *row_col, int *method, int *merge, double *height, int *dim, int *row_order, int *col_order, double *data)
{
	int row=dim[0];
	int col=dim[1];
	Link *rlnk = new Link[row];
	Link *clnk = new Link[col];
	double **rDist=new double*[row];
	for(int i=0; i<row; i++)
		rDist[i]= new double[row];
	
	int *rind = new int[2];
	double rmin;
	int rClus=1;
	bool *rvalid=new bool[row];
	int *rindex=new int[row];
	int *rsize=new int[row];
	for(int i=0; i<row; i++)
	{
		rvalid[i]=true;
		rindex[i] = -(i+1);
		rsize[i] = 1;
	}
	
	double **cDist=new double*[col];
	for(int i=0; i<col; i++)
		cDist[i]= new double[col];
	
	int *cind = new int[2];
	double cmin;
	int cClus=1;
	bool *cvalid=new bool[col];
	int *cindex=new int[col];
	int *csize=new int[col];
	for(int i=0; i<col; i++)
	{
		cvalid[i]=true;
		cindex[i] = -(i+1);
		csize[i] = 1;
	}
	bool rflag=true;
	bool cflag=true;
	rmin =0;
	float tmin=0;
	// tcmin=0;
	double ai, aj, ak;
	while(rClus+cClus<col+row)
	{
		if(rflag)
			PDist(data, rDist, row, col, true, rvalid, cvalid);
		if(cflag)
			PDist(data, cDist, col, row, false, cvalid, rvalid);

		// cout << "Row dist\n";
		// PrintDis(rDist, row, rvalid);
		// cout << "Col dist\n";
		// PrintDis(cDist, col, cvalid);		
		rmin=Min_Index(rDist, row, rind, rvalid);
		cmin=Min_Index(cDist, col, cind, cvalid);		
		if(rmin<cmin && rClus<row)
		{
			rlnk[rClus].L =rindex[rind[0]];
			rlnk[rClus].R =rindex[rind[1]];
			Mrg(rClus+cClus-2,0)=rindex[rind[0]];
			Mrg(rClus+cClus-2,1)=rindex[rind[1]];
			// height[rClus+cClus-2]=fabs(rmin);
			height[rClus+cClus-2]=rmin;
			if(rmin-tmin<0)
				height[rClus+cClus-2]=tmin+1;
				//height[rClus+cClus-2]=trmin+(trmin-rmin);
			tmin=height[rClus+cClus-2];

			row_col[rClus+cClus-2]=0;			
			ai=rsize[rind[0]];
			aj=rsize[rind[1]];
			for(int k=0; k<row; k++)		
				if(rvalid[k] && k!=rind[0] && k!=rind[1])
				{
					double dik=rDist[rind[0]][k];
					double djk=rDist[rind[1]][k];
					ak=rsize[k];
					switch(method[0])
					{
						case 1:
							rDist[rind[1]][k]=(ai/(ai+aj))*dik+(aj/(ai+aj))*djk;//Average
							break;
						
						case 2:
							rDist[rind[1]][k]=(ai/(ai+aj))*dik+(aj/(ai+aj))*djk-(ai*aj/pow((aj+ai),2))*rDist[rind[0]][rind[1]]; //Centroid
							break;

						case 3:
							rDist[rind[1]][k]=((ai+ak)/(ai+aj+ak))*dik+((aj+ak)/(ai+aj+ak))*djk-(ak/(aj+ai+ak))*rDist[rind[0]][rind[1]]; //Ward
							break;

						case 4:
							rDist[rind[1]][k]= .5*dik+.5*djk-.5*abs(dik-djk);//Single
							break;

						case 5:
							rDist[rind[1]][k]= .5*dik+.5*djk+.5*abs(dik-djk);//Complete
							break;
						default:
							rDist[rind[1]][k]=((ai+ak)/(ai+aj+ak))*dik+((aj+ak)/(ai+aj+ak))*djk-(ak/(aj+ai+ak))*rDist[rind[0]][rind[1]]; //Ward

					}
					//rDist[rind[1]][k]=(ai/(ai+aj))*dik+(aj/(ai+aj))*djk;//Average
					//rDist[rind[1]][k]=(ai/(ai+aj))*dik+(aj/(ai+aj))*djk-(ai*aj/pow((aj+ai),2))*rDist[rind[0]][rind[1]]; //Centroid
					//rDist[rind[1]][k]=((ai+ak)/(ai+aj+ak))*dik+((aj+ak)/(ai+aj+ak))*djk-(ak/(aj+ai+ak))*rDist[rind[0]][rind[1]]; //Ward
					rDist[k][rind[1]]=rDist[rind[1]][k];
				}
			for(int i=0; i<col; i++)
				M(rind[1],i)=(ai*M(rind[0], i)+aj*M(rind[1], i))/(ai+aj);//This should be checked later!
				// M(rind[1],i)=(M(rind[0], i)+M(rind[1], i))/2;//This should be checked later!
			rindex[rind[0]]=rClus;
			rindex[rind[1]]=rClus;
			rvalid[rind[0]]=false;
			rsize[rind[1]] = ai+aj;
			rflag=false;
			rClus++;
		}
		if(cmin < rmin && cClus<col)
		{
			clnk[cClus].L =cindex[cind[0]];
			clnk[cClus].R =cindex[cind[1]];
			Mrg(rClus+cClus-2,0)=cindex[cind[0]];
			Mrg(rClus+cClus-2,1)=cindex[cind[1]];
			height[rClus+cClus-2]=cmin;
			if(cmin-tmin<0)
				height[rClus+cClus-2]=tmin+1;
				//height[rClus+cClus-2]=tcmin+(tcmin-cmin);
			tmin=height[rClus+cClus-2];

			row_col[rClus+cClus-2]=1;			
			ai=csize[cind[0]];
			aj=csize[cind[1]];

			for(int k=0; k<col; k++)		
				if(cvalid[k] && k!=cind[0] && k!=cind[1])
				{
					double dik=cDist[cind[0]][k];
					double djk=cDist[cind[1]][k];
					ak=csize[k];
					switch(method[0])
					{
						case 1:
							cDist[cind[1]][k]=(ai/(ai+aj))*dik+(aj/(ai+aj))*djk;//Average
							break;
						
						case 2:
							cDist[cind[1]][k]=(ai/(ai+aj))*dik+(aj/(ai+aj))*djk-(ai*aj/pow((aj+ai),2))*cDist[cind[0]][cind[1]]; //Centroid
							break;

						case 3:
							cDist[cind[1]][k]=((ai+ak)/(ai+aj+ak))*dik+((aj+ak)/(ai+aj+ak))*djk-(ak/(aj+ai+ak))*cDist[cind[0]][cind[1]]; //Ward
							break;

						case 4:
							cDist[cind[1]][k]= .5*dik+.5*djk-.5*abs(dik-djk);//Single
							break;

						case 5:
							cDist[cind[1]][k]= .5*dik+.5*djk+.5*abs(dik-djk);//Complete
							break;

						default:
							cDist[cind[1]][k]=((ai+ak)/(ai+aj+ak))*dik+((aj+ak)/(ai+aj+ak))*djk-(ak/(aj+ai+ak))*cDist[cind[0]][cind[1]]; //Ward

					}
					//cDist[cind[1]][k]=(ai/(ai+aj))*dik+(aj/(ai+aj))*djk;//Average
					//cDist[cind[1]][k]=(ai/(ai+aj))*dik+(aj/(ai+aj))*djk-(ai*aj/pow((aj+ai),2))*cDist[cind[0]][cind[1]]; //Centroid
					//cDist[cind[1]][k]=((ai+ak)/(ai+aj+ak))*dik+((aj+ak)/(ai+aj+ak))*djk-(ak/(aj+ai+ak))*cDist[cind[0]][cind[1]]; //Ward
					cDist[k][cind[1]]=cDist[cind[1]][k];
				}
			for(int i=0; i<row; i++)
				M(i, cind[1])=(ai*M(i, cind[0])+aj*M(i, cind[1]))/(ai+aj);//This one as well
				// M(i, cind[1])=(M(i, cind[0])+M(i, cind[1]))/2;//This one as well
			cindex[cind[0]]=cClus;
			cindex[cind[1]]=cClus;
			cvalid[cind[0]]=false;
			csize[cind[1]] = ai+aj;
			cflag=false;
			cClus++;
		}
	}
	Extract(rlnk, row_order, rClus-1);
	Extract(clnk, col_order, cClus-1);	
}
}
/*
int main()
{	
	int row = 5;
	int col = 3;	
	double tmpd[row*col]={0.0,  3,  6,  9, 12,  1,  4,  7, 10, 13,  1,  5,  8, 11, 14};
	//double tmpd[row*col]={ 6.928418,  5.278878,  7.070719,  7.488934,  6.910724, 19.864575, 15.093598, 16.932269, 12.829218, 17.377856,  6.498859, 11.005977, 15.695510, 15.232849, 8.414224, 25.307194, 10.904425, 29.688645,  9.078612,  7.542631, 20.550562, 20.404695, 20.920979, 20.978992, 21.138890,  9.313876, 11.197479, 11.339983, 8.010185, 10.749250, 15.938705,  9.326312, 12.387474,  8.569716,  8.192885, 8.970814,  7.384553,  6.738385,  7.341049,  6.196665, 19.864564, 19.249710, 16.784207, 18.693000, 17.516185, 10.026302, 10.245642, 10.545728, 10.463930, 10.676803, 10.742611, 10.747771, 10.629414, 10.664170, 10.673522};
	//double tmpd[row*col]={1, 2, 3, 20, 22, 23, 30 };
	double *data = new double[row*col];
	for(int i=0;i<row*col; i++)
		data[i]=tmpd[i];
	
	for(int i=0; i<row; i++)
	{
		for(int j=0; j<col; j++)			
				cout<<M(i,j) << ' ';
		cout<<'\n';
	}
	
	double **rDist=new double*[row];
	for(int i=0; i<row; i++)
		rDist[i]= new double[row];

	double **cDist=new double*[row];
	for(int i=0; i<row; i++)
		cDist[i]= new double[row];

	int *ind = new int[2];

	int *combvec= new int[row+col-2];
	double *height= new double[row+col-2];
	int *merge = new int[2*(row+col-2)];	
	
	int *rorder=new int[row];
	int *corder=new int[col];

	int *rwcl=new int[2];
	rwcl[0]=row;
	rwcl[1]=col;
	
	// void Agl(int *row_col, int *merge, double *height, int *dim, int *row_order, int *col_order, double *data)
	Agl(combvec, merge, height, rwcl, rorder, corder, data);
	
	for(int i=0; i<row+col-2; i++)
		std::cout << combvec[i] << ',';
	std::cout <<'\n';

	std::cout<<"Merge and height\n";
	for(int i=0;i<row+col-2;i++)
		std::cout<<Mrg(i,0) << ',' << Mrg(i,1) << ':' << height[i] << '\n';	
	
	for(int i=0; i<row; i++)
		delete[] cDist[i];
	delete[] cDist;
	return 0;
}
*/