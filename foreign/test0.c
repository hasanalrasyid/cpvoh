#include <stdio.h>
extern double add_(int *, double [], int *, double []);

struct s_str {                                        
      double      size;                     
      double       adec;                    
      long long int       amode;               
      double       drwats;                  
      double       iinv[5];                 
      long long int       ivl;               
      double       kaps[6];                 
      long long int       lequiv;               
      long long int       lmaxw;               
      long long int       loka;               
      long long int       lshow;               
      long long int       mxnbr;               
      long long int       nalf;               
      long long int       nbisi[3];               
      long long int       ncupl;               
      long long int       ndust;               
      long long int       nitab;               
      long long int       nkaps;               
      long long int       nttab;               
      double       rfit;                    
      double       rmax;                    
      double       rmaxg;                   
      double       skmsh[6];                
      double       wztcf;                   
      };                       
struct s_site{                       
      int    iantiferro  ;                  
      double      size;                     
      double       bfield[3];               
      double       clabel  ;                
      long long int       class;               
      long long int       cli;               
      double       delta[6];                
      double       dpole[3];                
      double       eula[3];                 
      double       force[3];                
      double       mpole;                   
      long long int       ndelta;               
      long long int       norb;               
      long long int       pl;               
      long long int       plv;               
      double       pnu[20];                 
      double       pos[3];                  
      double       pos0[3];                 
      double       pz[20];                  
      long long int       relax[3];               
      long long int       sid;               
      long long int       spec;               
      double       vel[3];                  
      double       vshft;                   
      double       rv_a_ov0 [];               
      double       rv_a_ov1 [];               
      };                        
struct s_array{                       
      double      size;                     
//c      long long int       ncl;               
      long long int       nclasp;               
//c      long long int       nofgl;               
//c      long long int       nofgr;               
//c      long long int       npadl;               
//c      long long int       npadr;               
//c      long long int       npl;               
      double       rv_a_oclabl [];               
      int       iv_a_oipq [];               
      int       iv_a_oics [];               
      int     iv_a_oipc[];                  
      int      iv_a_oips [];                
      int       iv_a_onrc [];               
      int       iv_a_onrcp [];               
      double       rv_a_ormax [];               
      };                       
struct s_spec{                       
      double      size;                     
      double       z;                       
      double       mass;                    
      double       rmt;                     
      long long int       ntorb;               
      long long int       naug;               
      long long int       norb;               
      double       rsmfa;                   
      double       rsma;                    
      double       rg;                      
      long long int       lmxa;               
      long long int       kmxh;               
      long long int       lmxl;               
      long long int       kmxt;               
      double       rsmv;                    
      long long int       norp;               
      double       coreh  ;                 
      double       coreq[2];                
      double       a;                       
      long long int       nr;               
      double       eref;                    
      double       etf;                     
      double       beta;                    
      long long int       lfoca;               
      double       ctail;                   
      double       etail;                   
      double       name  ;                  
      double     rv_a_orhoc[];               
      double       stc;                     
      long long int       lmxb;               
      long long int       lmxf;               
      double       stni;                    
      double       stnm;                    
      double       rham;                    
      double       rfoca;                   
      double       dv;                      
      long long int       mxcst;               
      long long int       group;               
      long long int       grp2;               
      long long int       nxi;               
      double       qc;                      
      long long int       lmxpb;               
      double       pb1  ;                   
      double       pb2  ;                   
      double       colxbs[3];               
      long long int       lxi;               
      double       radxbs;                  
      double       rcut;                    
      double       rint;                    
      double       eh3;                     
      double       rs3;                     
      double       vmtz;                    
      long long int       kmxv;               
      double       rcfa[2];                 
      double       p[20];                   
      double       q[20];                   
      double       alpha[10];               
      long long int       idmod[10];               
      long long int       idxdn[30];               
      double       hcr[10];                 
      double       exi[10];                 
      long long int       ngcut[30];               
      double       qpol[10];                
      double       chfa[20];                
      double       orbp[60];                
      double       enu[10];                 
      double       pz[20];                  
      long long int       idu[4];               
      double       uh[4];                   
      double       jh[4];                   
      long long int       iq1[4];               
      long long int       ivso[4];               
      double       ehvl[10];                
      double       vso[4];                  
      int   nmcore  ;                       
      };                        
struct s_mix{                       
      double      size;                     
      double       b;                       
      double       bl;                      
      double       bv;                      
      double       elind;                   
      double       fn  ;                    
      int       kill;                       
      long long int       lxpot;               
      long long int       mmix;               
      long long int       mode;               
      long long int       model;               
      long long int       n;                
      long long int       nitu;               
      long long int       nmix;               
      long long int       nsave;               
      double       r[3]  ;                  
      double       rms1;                    
      double       rms2;                    
      double       tj[10];                  
      double       tolu;                    
      double       umix;                    
      double       w[3];                    
      double       wc;                      
      };                        
struct s_lat{                       
      double      size;                     
      double       alat;                    
      double       as;                      
      double       avw;                     
      double       awald;                   
      double       dist[9];                 
      double       gam[4];                  
      double       gmax;                    
      long long int       ldist;               
      long long int       nabc[3];               
      long long int       ng;               
      long long int       nkd;               
      long long int       nkdmx;               
      long long int       nkq;               
      long long int       nkqmx;               
      long long int       npgrp;               
      long long int       nsgrp;               
      double       rv_a_oag [];               
      struct {float r,i;} zv_a_obgv [];               
      double       rv_a_ocg [];               
      double       rv_a_ocy [];               
      double       rv_a_odlv [];               
      double       rv_a_ogv [];               
      int       iv_a_oidxcg [];               
      int       iv_a_oips0 [];               
      int       iv_a_oistab [];               
      int       iv_a_ojcg [];               
      int       iv_a_okv [];                
      double     rv_a_opos[];               
      double       rv_a_oqlv [];               
      double       rv_a_osymgr [];               
      double       plat[3][3];               
      double       plat0[3][3];               
      double       plat2[3][3];               
      double       plate[3][3];               
      double       platl[3][3];               
      double       platr[3][3];               
      double       qlat[3][3];               
      double       rpad;                    
      double       slat[3][3];               
      double       tol;                     
      double       tolft;                   
      double       vol;                     
      };                        
struct s_pot{                       
      double      size;                     
      double       bfield[4];               
      long long int       nlma;               
      long long int       nlml;               
      long long int       nrhos;               
      double       vconst[3];               
      double       vmtz;                    
      double       vmtz0;                   
      double       rv_a_obxc [];               
      double       rv_a_ogrrme [];               
      double       rv_a_omad [];               
      s_rv1     sv_a_oorhat[];               
      double       rv_a_opmpol [];               
      double       rv_a_opnu [];               
      double       rv_a_opp [];               
      double       rv_a_opprel [];               
      double       rv_a_oqnu [];               
      double       rv_a_oqpp [];               
      double       rv_a_oqt [];               
      double       rv_a_orhrmx [];               
      struct {float r,i;}        zv_a_osmpot;                   
      struct {float r,i;}        zv_a_osmrho;                   
      double       rv_a_osop [];               
      double       rv_a_ovdif [];               
      double       rv_a_oves [];               
      double       rv_a_ovintr [];               
      double       rv_a_ovrmax [];               
      };                        
                       
struct s_ham {                       
      double      size;                     
      double       alfsi;                   
      double       amgm;                    
      long long int       bandw;               
      double       dabc[3];                 
      double       ehf;                     
      double       ehk;                     
      double       elind;                   
      double       eterms[20];               
      long long int       hord;               
      double       kmto[6];                 
//      long long int       lasa;               
      long long int       ldham[16];               
      long long int       lgen3;               
      long long int       lham;               
//      long long int       lmaxu;               
      long long int       lmxax;               
//      long long int       lncol;               
      long long int       lsig;               
//      long long int       ltb;               
      long long int       lxcf;               
      long long int       nbf;               
      long long int       ndham;               
      long long int       ndhrs;               
//      long long int       ndofH;               
      long long int       neula;               
      long long int       nkaph;               
      long long int       nlibu;               
//      long long int       nmto;               
      long long int       npwmin;               
      long long int       npwpad;               
      long long int       nqsig;               
      double       oveps,delta_stabilize; //                              
//      ! H-> H+ delta_stabilize*O^-1;               
      double       pmax[10];                
      double       pmin[10];                
      double       pwemax;                  
      double       pwemin;                  
      long long int       pwmode;               
      double       qpoff[3];                
      double       qss[4];                  
      double       rsrnge;                  
      double       rsstol;                  
      double       seref;                   
      double       sigp[10];                
      double       thrpv;                   
      long long int       udiag;               
      double  scaledsigma;                  
      double     rv_a_ohrs [];               
      int   iv_a_oiaxs[];                   
      int iv_a_oindxo [];                   
      int    iv_a_ontabs[];                 
      int    iv_a_ooffH [];                 
      double rv_p_oqsig [] ;                
};

struct s_ctrl {                
  double     size;             
  double      defm[6];         
  double      elin;            
  long long int      lbas;     
  long long int      ldos;     
  long long int      lfp;      
  long long int      lfrce;    
  long long int      lgen3;    
  long long int      lham;     
  long long int      lmet;     
  long long int      lqp;      
  long long int      lrel;     
  long long int      lrs;      
  long long int      lscr;     
  long long int      lstr;     
  long long int      lsx;      
  long long int      lves;     
  long long int      lxcf;     
  long long int      maxit;    
  double      mdprm[6];        
  long long int      modep[3]; 
  long long int      nbas;     
  long long int      nbasp;    
  long long int      nclass;   
  long long int      nesabc[3];
  long long int      nitmv;    
  long long int      nl;       
  long long int      nmap;     
  long long int      nsite;    
  long long int      nspec;    
  long long int      nspin;    
  long long int      nvario;   
  double      omax1[3];        
  double      omax2[3];        
  long long int      pfloat;   
  long long int      quit;     
  double      rmaxes;          
  double      rmines;          
  double      sclwsr;          
  long long int      sdmod;    
  double      sdprm[5];        
  double      sdxsi[4];        
  long long int      smalit[2];
  double      tol[3];          
  double      wsrmax;          
  double      zbak[2];         
};                             

extern rdctrl2_ ( char *
                , int *
                , int *
                , char *
                , int *
                , char *
                , char *
                , struct s_ctrl *
                , struct s_ham *
                , struct s_pot *
                , struct s_lat *
                , struct s_mix *
                , struct s_spec *
                , struct s_site *
                , struct s_str *
                , struct s_array *
                , char *
                , char *);  //!,v_stb,v_smove !vrsion,vn,vn2,
//extern rdctrl2(recrd,recln,nrecs, prgnam, pass2,slabl_,v_sbz,v_sctrl,v_sham,v_spot,v_slat,v_smix,v_sspec,v_ssite, v_sstr,v_sarry,sstrnmix,sstrnsymg);  //!,v_stb,v_smove !vrsion,vn,vn2,


double ar1[4]={1.0, 2.0, 3.0, 4.0};
double ar2[4]={5.0, 6.0, 7.0, 8.0}; 

main()
{
  int x, y; 
  double z;

  x = 3;
  y = 3; 


  z = add_(&x, ar1, &y, ar2); /* Call Fortran add routine */ 
  /* Note: Fortran indexes arrays 1..n */
  /* C indexes arrays 0..(n-1) */ 

  printf("The sum of %1.0f and %1.0f is %2.0f \n",
      ar1[x-1], ar2[y-1], z);
}
