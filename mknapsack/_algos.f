      subroutine bld ( n6, ss, il, ir, ii, nxt, nlr )

c*********************************************************************72
c
cc BLD explicitly determines set  a - a1  or set a - a1 - a2  when lev .gt. 1 .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension ss(n6)

      j = il - 1
      next = nxt

      do i=il,ii,7
        ik = ss(next) + 1.
        i6 = i + 6
        do ij=ik,i6
          j = j + 1
          ss(j) = ss(ij)
        end do
        next = next + 1
      end do

      irr = ii + 7 + nlr

      do ij=irr,ir
        j = j + 1
        ss(j) = ss(ij)
      end do

      return
      end
      subroutine bldf(n,s,n6,ss,il,ir,nlr)

c*********************************************************************72
c
cc BLDF explicitly determines set  a1  or set  a - a1 or set  a - a1 - a2  when  lev .eq. 1 .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension s(n)
      dimension ss(n6)

      nn7 = (ir - il + 1)/7
      ics = 1
      icd = nn7 + 1
      ius = il + 6
      ips = ss(1) + 1.
      iud = il + 7*nn7
      ipd = iud + nlr - 1

   10 if ( ips .le. ius ) go to 20
      ics = ics + 1
      if ( ics .eq. icd ) return
      ius = ius + 7
      ips = ss(ics) + 1
      go to 10

   20 if ( ipd .ge. iud ) go to 30
      icd = icd - 1
      if ( icd .eq. ics ) return
      iud = iud - 7
      ipd = ss(icd)
      go to 20

   30 ap = s(ips)
      s(ips) = s(ipd)
      s(ipd) = ap
      ips = ips + 1
      ipd = ipd - 1
      go to 10

      end
      subroutine blds1(n6,ss,il,ii,nxt,nlr)

c*********************************************************************72
c
cc BLDSR1 explicitly determines set A1 when 1 < LEV.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension ss(n6)

      j = il - 1
      next = nxt

      do i=il,ii,7

        ik = ss(next)

        do ij=i,ik
          j = j + 1
          ss(j) = ss(ij)
        end do
        next = next + 1

      end do

      ik = ii + 7
      irr = ik + nlr - 1

      do ij=ik,irr
        j = j + 1
        ss(j) = ss(ij)
      end do

      return
      end
      subroutine chmt1(n,p,w,c,z,jdim)

c*********************************************************************72
c
cc CHMT1 checks the input data for MT1.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer c
      integer p(jdim)
      integer w(jdim)
      integer z

      if ( n .lt. 2 .or. jdim - 1 .lt. n ) then
        z = - 1
        return
      end if

      if ( c .gt. 0 ) go to 30
   20 z = - 2
      return
   30 jsw = 0
      rr = float(p(1))/float(w(1))

      do 50 j=1,n
        r = rr
        if ( p(j) .le. 0 ) go to 20
        if ( w(j) .le. 0 ) go to 20
        jsw = jsw + w(j)
        if ( w(j) .le. c ) go to 40
        z = - 3
        return
   40   rr = float(p(j))/float(w(j))
        if ( rr .le. r ) go to 50
        z = - 5
        return
   50 continue

      if ( jsw .gt. c ) return
      z = - 4

      return
      end
      subroutine chmt1r(n,p,w,c,z,jdim)

c*********************************************************************72
c
cc CHMT1R checks the input data for MT1R.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer jdim

      real jsw
      real p(jdim)
      real w(jdim)

      if ( n .lt. 2 .or. n .gt. jdim - 1 ) then
        z = - 1.0
        return
      end if

      if ( c .le. 0.0 ) then
        z = - 2.0
        return
      end if

      jsw = 0.0
      rr = p(1) / w(1)

      do j = 1, n

        r = rr

        if ( p(j) .le. 0. ) then
          z = - 2.0
          return
        end if

        if ( w(j) .le. 0. ) then
          z = - 2.0
          return
        end if

        jsw = jsw + w(j)

        if ( w(j) .gt. c ) then
          z = - 3.
          return
        end if

        rr = p(j)/w(j)

        if ( rr .gt. r ) then
          z = - 5.
          return
        end if

      end do

      if ( jsw .le. c ) then
        z = - 4.0
      end if

      return
      end
      subroutine chmt2(n,p,w,c,jfs,z,jdim)

c*********************************************************************72
c
cc CHMT2 checks the input data for MT2.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer c
      integer p(jdim),w(jdim),z

      if ( n .ge. 2 .and. n .le. jdim - 3 ) go to 10
      z = - 1
      return
   10 if ( c .gt. 0 ) go to 30
   20 z = - 2
      return
   30 jsw = 0
      do 40 j=1,n
        if ( p(j) .le. 0 ) go to 20
        if ( w(j) .le. 0 ) go to 20
        jsw = jsw + w(j)
        if ( w(j) .le. c ) go to 40
        z = - 3
        return
   40 continue
      if ( jsw .gt. c ) go to 50
      z = - 4
      return
   50 if ( jfs .eq. 0 ) return
      rr = float(p(1))/float(w(1))
      do 60 j=2,n
        r = rr
        rr = float(p(j))/float(w(j))
        if ( rr .gt. r ) go to 70
   60 continue
      return
   70 z = - 5
      return
      end
      subroutine chmtb2(n,p,w,b,c,jfs,z,jdim1)

c*********************************************************************72
c
cc CHMTB2 checks the input data for MTB2.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer c
      integer p(jdim1),w(jdim1),b(jdim1),z

      if ( n .gt. 1 .and. n .le. jdim1 - 1 ) go to 10
      z = - 1
      return
   10 if ( c .gt. 0 ) go to 30
   20 z = - 2
      return
   30 jsw = 0
      do 40 j=1,n
        if ( p(j) .le. 0 ) go to 20
        if ( w(j) .le. 0 ) go to 20
        if ( b(j) .le. 0 ) go to 20
        jsw = jsw + b(j)*w(j)
        if ( b(j)*w(j) .gt. c ) go to 50
   40 continue
      if ( jsw .gt. c ) go to 60
      z = - 4
      return
   50 z = - 3
      return
   60 if ( jfs .eq. 0 ) return
      rr = float(p(1))/float(w(1))
      do 70 j=2,n
        r = rr
        rr = float(p(j))/float(w(j))
        if ( rr .gt. r ) go to 80
   70 continue
      return
   80 z = - 6
      return
      end
      subroutine chmtc2(n,w,c,z,jdn)

c*********************************************************************72
c
cc CHMTC2 checks the input data for MTC2.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer c
      integer w(jdn),z

      if ( n .ge. 2 .and. n .le. jdn - 1 ) go to 10
      z = - 1
      return
   10 if ( c .gt. 0 ) go to 30
   20 z = - 2
      return
   30 do 40 j=1,n
        if ( w(j) .le. 0 ) go to 20
        if ( w(j) .ge. c ) go to 50
   40 continue
      return
   50 z = - 3
      return
      end
      subroutine chmtcb(n,w,b,c,z,jdn)

c*********************************************************************72
c
cc CHMTCB checks the input data for MTCB.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdn),b(jdn),c,z

      if ( n .ge. 2 .and. n .le. jdn - 1 ) go to 10
      z = - 1
      return
   10 if ( c .gt. 0 ) go to 30
   20 z = - 2
      return
   30 jsum = 0
      do 40 j=1,n
        if ( w(j) .le. 0 ) go to 20
        if ( b(j) .le. 0 ) go to 20
        if ( w(j) .ge. c ) go to 50
        if ( b(j)*w(j) .gt. c ) go to 60
        jsum = jsum + b(j)*w(j)
   40 continue
      if ( jsum .le. c ) go to 70
      return
   50 z = - 3
      return
   60 z = - 4
      return
   70 z = - 5
      return
      end
      subroutine chmtg(n,m,p,w,c,jdimr,jdimc,jdimpc,z)

c*********************************************************************72
c
cc CHMTG checks the input data for MTG.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),c(jdimr),z

      if ( m .le. 1 ) z = - 1
      if ( m .gt. jdimr ) z = - 1
      if ( z .lt. 0 ) return
      if ( n .le. 1 ) z = - 2
      if ( n .gt. jdimc ) z = - 2
      if ( z .lt. 0 ) return
      if ( m .le. jdimpc) go to 10
      z = - 3
      return
   10 do 50 i=1,m
        if ( c(i) .gt. 0 ) go to 20
        z = - 4
        return
   20   min = c(i) + 1
        do 40 j=1,n
          if ( p(i,j) .gt. 0 .and. w(i,j) .gt. 0 ) go to 30
          z = - 4
          return
   30     if ( w(i,j) .lt. min ) min = w(i,j)
   40   continue
        if ( c(i) .lt. min ) z = - 6
   50 continue
      do 70 j=1,n
        do 60 i=1,m
          if ( w(i,j) .le. c(i) ) go to 70
   60   continue
        z = - 5
        return
   70 continue
      return
      end
      subroutine chmthg(n,m,p,w,c,jdimr,jdimc,z)

c*********************************************************************72
c
cc CHMTHG checks the input data for MTHG.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),c(jdimr),z

      if ( m .le. 1 ) z = - 1
      if ( m .gt. jdimr ) z = - 1
      if ( z .lt. 0 ) return
      if ( n .le. 1 ) z = - 2
      if ( n .gt. jdimc ) z = - 2
      if ( z .lt. 0 ) return
      do 40 i=1,m
        if ( c(i) .gt. 0 ) go to 10
        z = - 3
        return
   10   min = c(i) + 1
        do 30 j=1,n
          if ( p(i,j) .gt. 0 .and. w(i,j) .gt. 0 ) go to 20
          z = - 3
          return
   20     if ( w(i,j) .lt. min ) min = w(i,j)
   30   continue
        if ( c(i) .lt. min ) z = - 5
   40 continue
      do 60 j=1,n
        do 50 i=1,m
          if ( w(i,j) .le. c(i) ) go to 60
   50   continue
        z = - 4
        return
   60 continue
      return
      end
      subroutine chmthm ( n, m, p, w, c, jdn, jdm, z )

c*********************************************************************72
c
cc CHMTHM checks the input data for MTHM.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdn),w(jdn),c(jdm),z

      if ( n .le. 1 ) z = - 1
      if ( n .ge. jdn ) z = - 1
      if ( m .le. 0 ) z = - 1
      if ( m .ge. jdm ) z = - 1
      if ( z .lt. 0 ) return
      maxw = w(1)
      minw = w(1)
      isumw = 0
      rr = p(1)

      do j=1,n

        if ( p(j) .le. 0 ) z = - 2
        if ( w(j) .le. 0 ) z = - 2
        if ( z .lt. 0 ) return
        if ( w(j) .gt. maxw ) maxw = w(j)
        if ( w(j) .lt. minw ) minw = w(j)
        isumw = isumw + w(j)
        r = rr
        rr = float(p(j))/float(w(j))
        if ( rr .gt. r ) then
          z = - 6
          return
        end if

      end do

      if ( c(1) .le. 0 ) z = - 2

      do i = 2, m
        if ( c(i) .le. 0 ) z = - 2
        if ( c(i) .lt. c(i-1) ) then
          z = - 7
          return
        end if
      end do

      if ( minw .gt. c(1) ) z = - 3
      if ( maxw .gt. c(m) ) z = - 4
      if ( isumw .le. c(m) ) z = - 5

      return
      end
      subroutine chmtm(n,m,p,w,c,maxn,maxm,z)

c*********************************************************************72
c
cc CHMTM checks the input data for MTM.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(1000),w(1000),c(10),z

      if ( n .le. 1 ) z = - 1
      if ( n .gt. maxn ) z = - 1
      if ( m .le. 0 ) z = - 1
      if ( m .gt. maxm ) z = - 1
      if ( z .lt. 0 ) return
      maxw = w(1)
      minw = w(1)
      isumw = 0
      rr = p(1)
      do 10 j=1,n
        if ( p(j) .le. 0 ) z = - 2
        if ( w(j) .le. 0 ) z = - 2
        if ( z .lt. 0 ) return
        if ( w(j) .gt. maxw ) maxw = w(j)
        if ( w(j) .lt. minw ) minw = w(j)
        isumw = isumw + w(j)
        r = rr
        rr = float(p(j))/float(w(j))
        if ( rr .le. r ) go to 10
        z = - 6
        return
   10 continue
      if ( c(1) .le. 0 ) z = - 2
      if ( m .eq. 1 ) go to 30
      do 20 i=2,m
        if ( c(i) .le. 0 ) z = - 2
        if ( c(i) .ge. c(i-1) ) go to 20
        z = - 7
        return
   20 continue
   30 if ( minw .gt. c(1) ) z = - 3
      if ( maxw .gt. c(m) ) z = - 4
      if ( isumw .le. c(m) ) z = - 5
      return
      end
      subroutine chmtp(n,w,c,jdim,z)

c*********************************************************************72
c
cc CHMTP checks the input data for MTP.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim),c,z
      if ( n .ge. 2 .and. n .le. jdim ) go to 10
      z = - 1
      return
   10 if ( c .gt. 0 ) go to 30
   20 z = - 2
      return
   30 jwp = w(1)
      do 60 j=1,n
        if ( w(j) .le. 0 ) go to 20
        if ( w(j) .le. c ) go to 40
        z = - 3
        return
   40   if ( w(j) .le. jwp ) go to 50
        z = - 4
        return
   50   jwp = w(j)
   60 continue
      return
      end
      subroutine chmtsl(n,w,c,z,jdn)

c*********************************************************************72
c
cc CHMTSL checks the input data for MTSL.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdn),c,z

      if ( n .ge. 2 .and. n .le. jdn - 1 ) go to 10
      z = - 1
      return
   10 if ( c .gt. 0 ) go to 30
   20 z = - 2
      return
   30 jsw = 0
      do 40 j=1,n
        if ( w(j) .le. 0 ) go to 20
        jsw = jsw + w(j)
        if ( w(j) .lt. c ) go to 40
        z = - 3
        return
   40 continue
      if ( jsw .gt. c ) return
      z = - 4
      return
      end
      subroutine chmtu2(n,p,w,c,z,jdim)

c*********************************************************************72
c
cc CHMTU2 checks the input data for MTU2.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdim),w(jdim),c,z

      if ( n .gt. 1 .and. n .le. jdim - 1 ) go to 10
      z = - 1
      return
   10 if ( c .gt. 0 ) go to 30
   20 z = - 2
      return
   30 do 40 j=1,n
        if ( p(j) .le. 0 ) go to 20
        if ( w(j) .le. 0 ) go to 20
        if ( w(j) .gt. c ) go to 50
   40 continue
      return
   50 z = - 3
      return
      end
      subroutine cmpb(n,w,b,c,z,x,jdn,jdl,maxbck,xx,m,l)

c*********************************************************************72
c
cc CMPB solves a bounded change-making problem through the branch-and-bound algorithm.
c
c presented in
c  s. martello, p. toth, "solution of the bounded and unbounded change-
c  making problem", tims/orsa joint national meeting, san francisco,
c  1977.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdn),b(jdn),x(jdn),c,z
      integer xx(jdn),cwf,profit,cws
      integer m(jdl),l(jdl)
c
c step 1.
c
      kbck = 0
      cwf = c
      w(n+1) = 1
      b(n+1) = c + 1
c
c lower bound computation.
c
      cws = c
      jsb = 0
      do j=1,n
        if ( b(j)*w(j) .gt. cws )go to 20
        cws = cws - b(j)*w(j)
        jsb = jsb + b(j)
      end do
   20 js = j
      jzp = jsb + cws/w(js)
      jcp = cws - (cws/w(js))*w(js)
      lb = jzp + ( jcp + w(js+1) - 1)/w(js+1)
      lb1 = jzp - 1 + ( jcp + w(js-1) + w(js) - 1 )/w(js)
      if ( lb1 .lt. lb ) lb = lb1
      if ( jcp .gt. 0 ) go to 50
      z = jzp
      do 30 j=1,js
        x(j) = b(j)
   30 continue
      do 40 j =js,n
        x(j) = 0
   40 continue
      x(js) = cws/w(js)
      return
   50 jdom = jdl
      if ( jdom .ge. w(1) ) jdom = w(1) - 1
      if ( jdom .lt. w(n) ) go to 80
      n2 = n + 2
      do 70 jj=2,n
        j = n2 - jj
        k1 = w(j)
        k2 = w(j-1) - 1
        if ( k2 .gt. jdom ) k2 = jdom
        do 60 k=k1,k2
          m(k) = j
          l(k) = 0
   60   continue
      if ( k2 .eq. jdom ) go to 80
   70 continue
   80 k1 = w(n) - 1
      if ( k1 .gt. jdom ) k1 = jdom
      if ( k1 .eq. 0 ) go to 100
      do 90 k=1,k1
        l(k) = z
   90 continue
  100 xx(1) = c/w(1)
      if ( b(1) .lt. xx(1) ) xx(1) = b(1)
      do 110 j=2,n
        xx(j) = 0
  110 continue
      profit = xx(1)
      c = c - xx(1)*w(1)
      ii = 2
      go to 150
c
c step (2.a).
c
  120 if ( c .le. jdom ) go to 140
      if ( c .lt. w(n) ) go to 230
      iiold = ii
  130 if ( c .ge. w(ii) ) go to 150
      ii = ii + 1
      go to 130
  140 if ( l(c) .ge. z - profit ) go to 230
      iiold = ii
      ii = m(c)
c
c step 2.
c
  150 jyp = 0
      jct = c
      do 160 i =ii,n
        jy = jct/w(i)
        if ( jy .gt. b(i) ) jy = b(i)
        jyp = jyp + jy
        jct = jct - jy*w(i)
        jw = w(i+1)
        if ( jy .lt. b(i) ) jw = w(i)
        if ( z .le. profit + jyp + (jct + jw - 1)/jw ) go to 230
        if ( jct .eq. 0 ) go to 200
        if ( jy .lt. b(i) ) go to 170
  160 continue
      go to 230
  170 profit = profit + ( jyp - jy )
      i1 = i - 1

      do k=ii,i1
        xx(k) = b(k)
      end do

  190 ii = i
c
c step 3.
c
      c = jct
      profit = profit + jy
      xx(ii) = jy
      ii = ii + 1
      go to 120
c
c step 4.
c
  200 z = profit + jyp

      do j=1,n
        x(j) = xx(j)
      end do

      do j=ii,i
        x(j) = b(j)
      end do

      x(i) = jy
      if ( z .ne. lb ) go to 230
      c = cwf
      return
c
c step 5.
c
  230 kbck = kbck + 1
      if ( kbck .eq. maxbck ) go to 250
      ib = ii - 1
      do 240 j=1,ib
        iij = ii - j
        if ( xx(iij) .gt. 0 ) go to 260
  240 continue
  250 c = cwf
      if ( z .gt. c ) z = 0
      return
  260 kk = ii - j
      if ( c .ge. w(kk) ) go to 270
      if ( c .gt. jdom ) go to 270
      if ( z - profit .gt. l(c) ) l(c) = z - profit
  270 c = c + w(kk)
      profit = profit - 1
      xx(kk) = xx(kk) - 1
      if ( z .gt. profit + (c + w(kk+1) - 1)/w(kk+1) ) go to 280
      c = c + xx(kk)*w(kk)
      profit = profit - xx(kk)
      xx(kk) = 0
      ii = kk + 1
      go to 230
  280 ii = kk + 1
      iiold = ii
      if ( c .gt. jdom ) go to 290
      if ( l(c) .ge. z - profit ) go to 230
  290 if ( c - w(kk) .ge. w(n) ) go to 150
      ih = kk
c
c step 6.
c
  300 ih = ih + 1
      if ( z .le. profit + (c + w(ih) - 1)/w(ih) ) go to 230
      if ( ih .gt. n ) go to 230
      if ( c - w(ih) .lt. w(n) ) go to 300
      ii = ih
      iiold = ii
      go to 150
      end
      subroutine core(n,p,w,c,jfo,iz1,icw,minw0,jdim,ff,nnf,nf,fn1,
     &                fn0,pw)

c*********************************************************************72
c
cc CORE determines the core problem.
c
c nf(j) = successor of item j in the corresponding set;
c fn1 (ln1) = pointer to the first (last) item in set n1 (fixed to 1);
c fn0 (ln0) = pointer to the first (last) item in set n0 (fixed to 0);
c fnf (lnf) = pointer to the first (last) item in set nf (free items);
c the set of free items is partitioned into 3 sets:
c    ng = items with ratio .gt. lambda,
c    nl = items with ratio .lt. lambda,
c    ne = items with ratio .eq. lambda;
c nf(n+1) (nf(n+2),nf(n+3)) = pointer to the first item in ng (nl,ne);
c lng (lnl,lne) = pointer to the last item in ng (nl,ne);
c modng (modnl,modne) = number of items in ng (nl,ne).
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdim),w(jdim),ff(jdim),nf(jdim)
      integer c,s1,s2,fnf,fn0,fn1,se,teta,ss
      real    pw(jdim),lambda
c
c step 0 (initialize).
c
      do j=1,n
        pw(j) = float(p(j))/float(w(j))
      end do
      if ( jfo .eq. 0 ) go to 20
      if ( n .ge. 200 ) go to 20
      nnf = n
      return
   20 teta = 2.0*sqrt(float(n))
      if ( jfo .eq. 0 ) teta = 5
      keta = 20
      if ( jfo .eq. 0 ) keta = 200
      alpha = 0.2
      if ( jfo .eq. 0 ) alpha = 0.
      beta = 1.
      fn0 = n + 1
      ln0 = n + 1
      fn1 = n + 1
      ln1 = n + 1
      fnf = 1
      lnf = n
      do j=1,n
        nf(j) = j + 1
      end do
      np1 = n + 1
      modnf = n
      ln0p = n + 1
      ln1p = n + 1
      ls1 = 0
      klam = 0
c
c step 1.
c
c choose lambda (median of the ratios of 3 items in nf).
   40 lambda = fmed(nf,jdim,pw,n,fnf,lnf)
c
c step 2.
c
c define ng, nl, ne;
c compute s1 = sum of weights of items in (n1 union ng);
c compute s2 = sum of weights of items in (n1 union ng union ne).
c
   50 klam = klam + 1
      if ( klam .gt. keta ) go to 440
      s1 = ls1
      se = 0
      lng = n + 1
      nf(lng) = np1
      lnl = n + 2
      nf(lnl) = np1
      lne = n + 3
      nf(lne) = np1
      nf(lnf) = np1
      j = fnf
      modng = 0
      modne = 0
   60 if ( pw(j) - lambda ) 70,80,90
   70 nf(lnl) = j
      lnl = j
      go to 100
   80 nf(lne) = j
      lne = j
      se = se + w(j)
      modne = modne + 1
      go to 100
   90 nf(lng) = j
      lng = j
      s1 = s1 + w(j)
      modng = modng + 1
  100 j = nf(j)
      if ( j .le. n ) go to 60
      s2 = s1 + se
      if ( s1 .gt. c ) go to 310
      if ( s2 .le. c ) go to 370
c
c lambda has been found.
c
      if ( modne .ge. teta ) go to 110
      if ( modng + modne .lt. teta ) go to 470
      go to 460
c
c step 2.1 (add ng to n1 and nl to n0).
c
  110 if ( fn1 .le. n ) go to 120
      fn1 = nf(n+1)
      go to 130
  120 nf(ln1) = nf(n+1)
  130 if ( fn0 .le. n ) go to 140
      fn0 = nf(n+2)
      go to 150
  140 nf(ln0) = nf(n+2)
  150 jfpr = 2
      if ( nf(n+1) .gt. n ) go to 160
      ln1p = ln1
      ln1 = lng
  160 if ( nf(n+2) .gt. n ) go to 170
      ln0p = ln0
      ln0 = lnl
  170 nf(ln1) = np1
      nf(ln0) = np1
      nout = 0
      if ( modne .eq. teta ) go to 260
c
c step 2.2 (subset-sum type problems).
c
      j = nf(n+3)
      ss = s1
      ness = 0
  180 ss = ss + w(j)
      if ( ss .gt. c ) go to 190
      ness = ness + 1
      j = nf(j)
      go to 180
  190 if ( ness .le. teta/2 ) go to 260
      if ( modne - ness .le. teta/2 ) go to 200
      nout = ness - teta/2
      go to 210
  200 nout = modne - teta
c
c insert in set n1 the first nout elements of set ne.
c
  210 j = nf(n+3)
      ness = 0
  220 ness = ness + 1
      if ( ness .eq. nout ) go to 230
      j = nf(j)
      go to 220
  230 if ( fn1 .le. n ) go to 240
      fn1 = nf(n+3)
      go to 250
  240 nf(ln1) = nf(n+3)
  250 ln1 = j
      nf(n+3) = nf(j)
      nf(ln1) = np1
c
c define the core problem.
c
  260 k = 0
      nf(lne) = np1
      j = nf(n+3)
  270 k = k + 1
      ff(k) = j
      if ( k .eq. teta ) go to 280
      j = nf(j)
      go to 270
  280 if ( modne .eq. nout + teta ) go to 530
c
c insert in set n0 the last elements of set ne.
c
      j = nf(j)
      if ( fn0 .le. n ) go to 290
      fn0 = j
      go to 300
  290 nf(ln0) = j
  300 ln0 = lne
      go to 530
c
c step 3 (lambda is too small).
c
  310 if ( float(modng) .lt. float(teta)*(1. - alpha) ) go to 470
c
c set nf equal to ng.
c
      fnf = nf(n+1)
      lnf = lng
      modnf = modng
c
c add (nl union ne) to n0.
c
      ln0p = ln0
      jfpr = 0
      if ( fn0 .le. n ) go to 330
      if ( nf(n+2) .le. n ) go to 320
      fn0 = nf(n+3)
      go to 360
  320 fn0 = nf(n+2)
      go to 350
  330 if ( nf(n+2) .le. n ) go to 340
      nf(ln0) = nf(n+3)
      go to 360
  340 nf(ln0) = nf(n+2)
  350 nf(lnl) = nf(n+3)
  360 ln0 = lne
      go to 430
c
c step 4 (lambda is too large).
c
  370 modnl = modnf - modng - modne
      if ( float(modnl) .lt. float(teta)*(1. - alpha) ) go to 460
c
c set nf equal to nl.
c
      fnf = nf(n+2)
      lnf = lnl
      modnf = modnl
c
c add (ng union ne) to n1.
c
      ln1p = ln1
      jfpr = 1
      if ( fn1 .le. n ) go to 390
      if ( nf(n+1) .le. n ) go to 380
      fn1 = nf(n+3)
      go to 420
  380 fn1 = nf(n+1)
      go to 410
  390 if ( nf(n+1) .le. n ) go to 400
      nf(ln1) = nf(n+3)
      go to 420
  400 nf(ln1) = nf(n+1)
  410 nf(lng) = nf(n+3)
  420 ln1 = lne
      ls1 = s2
  430 if ( float(modnf) .gt. (1. + beta)*float(teta) ) go to 40
c
c step 5 (set the core problem equal to nf).
c
  440 j = fnf
      nf(lnf) = np1
      k = 0
  450 k = k + 1
      ff(k) = j
      j = nf(j)
      if ( j .le. n ) go to 450
      go to 530
c
c step 6 (update lambda).
c
c lambda = median of the ratios of 3 items in ng.
  460 jfret = 1
      if ( modng .lt. 3 ) go to 480
      jfret = 0
      lambda = fmed(nf,jdim,pw,n,nf(n+1),lng)
      go to 480
c
c lambda = median of the ratios of 3 items in nl.
c
  470 jfret = 1
      modnl = modnf - modng - modne
      if ( modnl .lt. 3 ) go to 480
      jfret = 0
      lambda = fmed(nf,jdim,pw,n,nf(n+2),lnl)
c
c re-define the previous set of free items ( = ne union ng union nl ).
c
  480 fnf = nf(n+3)
      if ( nf(n+1) .le. n ) go to 500
      if ( nf(n+2) .le. n ) go to 490
      lnf = lne
      go to 520
  490 nf(lne) = nf(n+2)
      lnf = lnl
      go to 520
  500 nf(lne) = nf(n+1)
      if ( nf(n+2) .le. n ) go to 510
      lnf = lng
      go to 520
  510 nf(lng) = nf(n+2)
      lnf = lnl
  520 if ( jfret .eq. 1 ) go to 440
      go to 50
c
c step 7.
c
  530 nnf = k
c
c compute iz1 and icw.
c
      iz1 = 0
      icw = c
      if ( fn1 .gt. n ) go to 550
      j = fn1
      nf(ln1) = np1
  540 if ( j .gt. n ) go to 550
      iz1 = iz1 + p(j)
      icw = icw - w(j)
      j = nf(j)
      go to 540
c
c compute minw0.
c
  550 minw0 = 10*c
      if ( fn0 .gt. n ) go to 570
      j = fn0
      nf(ln0) = np1
  560 if ( j .gt. n ) go to 570
      if ( w(j) .lt. minw0 ) minw0 = w(j)
      j = nf(j)
      go to 560
c
c add items to the core problem until the maximum weight
c in core is .le. icw .
c
  570 maxwc = 0
      do k=1,nnf
        j = ff(k)
        if ( w(j) .gt. maxwc ) maxwc = w(j)
      end do
  590 if ( maxwc .le. icw ) go to 640
      j = fn1
      pwmin = pw(j)
      jmin = j
      jminp = 0
  600 if ( j .gt. n ) go to 620
      if ( pw(j) .ge. pwmin ) go to 610
      pwmin = pw(j)
      jmin = j
      jminp = jp
  610 jp = j
      j = nf(j)
      go to 600
  620 nnf = nnf + 1
      ff(nnf) = jmin
      iz1 = iz1 - p(jmin)
      icw = icw + w(jmin)
      if ( jminp .ne. 0 ) go to 630
      fn1 = nf(jmin)
      go to 590
  630 nf(jminp) = nf(jmin)
      go to 590
  640 return
      end
      subroutine corec(n,w,i1,i2,i3,jdn,nc,pr)

c*********************************************************************72
c
cc COREC determines the core problem.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdn),pr(jdn)
      integer iv(3),ivs(3)
      nc = n/20
      if ( nc .lt. 500 ) nc = 500
      iv(1) = i1
      pr(1) = 1
      iv(2) = i2
      pr(2) = 2
      iv(3) = i3
      pr(3) = 3
      call sorti(3,iv,pr,3)
      ivs(1) = - iv(pr(3))
      ivs(2) = - iv(pr(2))
      ivs(3) = - iv(pr(1))
      jtest = 1
      jvtest = - ivs(1)
      lli = 1
      do 30 j=1,nc
   10   if ( lli .lt. jvtest ) go to 20
          if ( lli .eq. jvtest ) ivs(jtest) = - ivs(jtest)
          jtest = jtest + 1
          jvtest = n + 1
          if ( jtest .le. 3 ) jvtest = - ivs(jtest)
        go to 10
   20   pr(j) = lli
        if ( j .lt. nc ) lli = lli + (n - lli)/(nc - j)
   30 continue
      nt = nc + 1
      if ( ivs(1) .gt. 0 ) go to 40
      nt = nt - 1
      if ( ivs(2) .eq. pr(nt) .or. ivs(3) .eq. pr(nt) ) nt = nt - 1
      if ( ivs(2) .eq. pr(nt) .or. ivs(3) .eq. pr(nt) ) nt = nt - 1
      pr(nt) = - ivs(1)
   40 if ( ivs(2) .gt. 0 ) go to 50
      nt = nt - 1
      if ( ivs(1) .eq. pr(nt) .or. ivs(3) .eq. pr(nt) ) nt = nt - 1
      if ( ivs(1) .eq. pr(nt) .or. ivs(3) .eq. pr(nt) ) nt = nt - 1
      pr(nt) = - ivs(2)
   50 if ( ivs(3) .gt. 0 ) return
      nt = nt - 1
      if ( ivs(1) .eq. pr(nt) .or. ivs(2) .eq. pr(nt) ) nt = nt - 1
      if ( ivs(1) .eq. pr(nt) .or. ivs(2) .eq. pr(nt) ) nt = nt - 1
      pr(nt) = - ivs(3)
      return
      end
      subroutine cores(n,p,w,c,jfo,iz1,icw,minw0,jdim,ff,nnf,nf,fn1,
     &                 fn0,pw)

c*********************************************************************72
c
cc CORES determines the core problem.
c
c  Discussion:
c
c    It is assumed the items are already sorted
c    according to decreasing profit per unit weight.
c
c    nf(j) = successor of item j in the corresponding set;
c    fn1 = pointer to the first item in set n1 (fixed to 1);
c    fn0 = pointer to the first item in set n0 (fixed to 0);
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdim),w(jdim),ff(jdim),nf(jdim)
      integer c,fn0,fn1,teta
      real pw(jdim)
c
c find the critical item.
c
      if ( jfo .eq. 0 ) go to 10
      if ( n .ge. 200 ) go to 10
      nnf = n
      return
   10 teta = 2.0*sqrt(float(n))
      if ( jfo .eq. 0 ) teta = 8
      if ( teta .gt. n ) teta = n
      ic = c
      do 20 j=1,n
        if ( w(j) .gt. ic ) go to 30
        ic = ic - w(j)
   20 continue
c
c no exit this way.
c
   30 ll = j - 1
c
c define the core problem.
c
      ll1 = ll - teta/2
      if ( ll1 .lt. 1 ) ll1 = 1
      ll2 = ll1 + teta - 1
      if ( ll2 .le. n ) go to 40
      ll2 = n
      ll1 = n - teta + 1
   40 nnf = teta
      iz1 = 0
      icw = c
      ll1m1 = ll1 - 1
      if ( ll1m1 .gt. 0 ) go to 50
      fn1 = n + 1
      go to 70
   50 fn1 = 1

      do j=1,ll1m1
        iz1 = iz1 + p(j)
        icw = icw - w(j)
        nf(j) = j + 1
      end do

      nf(ll1m1) = n + 1
   70 maxwc = 0

      do j=1,teta
        jj = j + ll1m1
        ff(j) = jj
        if ( w(jj) .gt. maxwc ) maxwc = w(jj)
      end do

      if ( maxwc .le. icw ) go to 110
      j = ll1
   90 j = j - 1
        iz1 = iz1 - p(j)
        icw = icw + w(j)
        nnf = nnf + 1
        ff(nnf) = j
      if ( maxwc .gt. icw ) go to 90
      if ( j .gt. 1 ) go to 100
      fn1 = n + 1
      go to 110
  100 nf(j-1) = n + 1
  110 minw0 = 10*c
      ll2p1 = ll2 + 1
      if ( ll2p1 .le. n ) go to 120
      fn0 = n + 1
      go to 140
  120 fn0 = ll2p1
      do 130 j=ll2p1,n
        if ( w(j) .lt. minw0 ) minw0 = w(j)
        nf(j) = j + 1
  130 continue
  140 do 150 j=1,nnf
        jj = ff(j)
        pw(jj) = float(p(jj))/float(w(jj))
  150 continue
      return
      end
      subroutine defpck(m,jdimpc)

c*********************************************************************72
c
cc DEFPCK defines the vectors for packing.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer y
      common /pack/ mask1(30),itwo(30),mask,y(150,100)
      do i=1,m
        itwo(i) = 2**(i-1)
        mask1(i) = 2**(jdimpc) - 1 - itwo(i)
      end do
      mask = 1
      return
      end
      subroutine detns1(na,a,n6,ss,il,ir,ii,nxt,v,ns1,nlr)

c*********************************************************************72
c
cc DETNS1 computes the cardinality of set A1.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension a(na),ss(n6)
      ns1 = 0
      next = nxt
      do 90 i=il,ii,7
        if ( a(i+3) .lt. v ) go to 40
        if ( a(i+1) .lt. v ) go to 20
        if ( a(i) .lt. v ) go to 10
        ss(next) = i - 1
        go to 80
   10   ns1 = ns1 + 1
        ss(next) = i
        go to 80
   20   if ( a(i+2) .lt. v ) go to 30
        ns1 = ns1 + 2
        ss(next) = i + 1
        go to 80
   30   ns1 = ns1 + 3
        ss(next) = i + 2
        go to 80
   40   if ( a(i+5) .lt. v ) go to 60
        if ( a(i+4) .lt. v ) go to 50
        ns1 = ns1 + 4
        ss(next) = i + 3
        go to 80
   50   ns1 = ns1 + 5
        ss(next) = i + 4
        go to 80
   60   if ( a(i+6) .lt. v ) go to 70
        ns1 = ns1 + 6
        ss(next) = i + 5
        go to 80
   70   ns1 = ns1 + 7
        ss(next) = i + 6
   80   next = next + 1
   90 continue
      nlr = 0
      irr = ii + 7
      if ( irr .gt. ir ) return
      do i=irr,ir
        if ( a(i) .ge. v ) go to 110
        nlr = nlr + 1
      end do
  110 ns1 = ns1 + nlr
      return
      end
      subroutine detns2(na,a,n6,ss,il,ir,ii,nxt,v,ns2,nlr)

c*********************************************************************72
c
cc DETNS2 computes the cardinality of set A2.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension a(na),ss(n6)
      ns2 = 0
      next = nxt
      do 50 i=il,ii,7
        is = ss(next) + 1.
        i6 = i + 6
        if ( is .le. i6 ) go to 10
        ss(next) = i6
        go to 40
   10   do 20 j=is,i6
          if ( a(j) .gt. v ) go to 30
   20   continue
        ns2 = ns2 + i6 - is + 1
        ss(next) = i6
        go to 40
   30   ns2 = ns2 + j - is
        ss(next) = j - 1
   40   next = next + 1
   50 continue
      irr = ii + 7 + nlr
      if ( irr .gt. ir ) return
      ner = 0
      do 60 i=irr,ir
        if ( a(i) .gt. v ) go to 70
        ner = ner + 1
   60 continue
   70 ns2 = ns2 + ner
      nlr = nlr + ner
      return
      end
      subroutine dinsm(n,w,cd,m2,jdd,td1,td2,td3,nsds,nsdm,m,
     &                 jflm,jfls,pers)

c*********************************************************************72
c
cc DINSM determines the dynamic programming lists.
c
c  Discussion:
c
c    for each state  j ,  td*(j,1)  gives the weight,  td*(j,2) gives
c    the corresponding bit string.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(n),cd
      integer td1(jdd,2),td2(jdd,2),td3(jdd,2)
      jdds = jdd
      jddm = jdd
      k = n
      td1(1,1) = 0
      td1(1,2) = 0
      td1(2,1) = w(n)
      td1(2,2) = 1
      jcall = 1
      nsds = 2
      nsdm = 0
      jbit = 1
      jsumw = w(n)
   10 k = k - 1
      jsumw = jsumw + w(k)
      if ( k .lt. n - m2 + 1 ) go to 120
      if ( w(k) .gt. cd ) go to 120
      jbit = jbit*2
      nsdso = nsds
      if ( jcall .eq. 2 ) go to 20
      if ( jcall .eq. 3 ) go to 30
      if ( jcall .eq. 4 ) go to 40
      if ( jcall .eq. 5 ) go to 50
      if ( jcall .eq. 6 ) go to 60
      call tab(td1,td2,nsds,w(k),cd,jdds,jdd,jbit,jflag)
      jcall = 2
      go to 70
   20 call tab(td2,td1,nsds,w(k),cd,jdds,jdd,jbit,jflag)
      jcall = 1
      go to 70
   30 call tab(td1,td3,nsds,w(k),cd,jddm,jdd,jbit,jflag)
      jcall = 4
      go to 70
   40 call tab(td3,td1,nsds,w(k),cd,jddm,jdd,jbit,jflag)
      jcall = 3
      go to 70
   50 call tab(td2,td3,nsds,w(k),cd,jddm,jdd,jbit,jflag)
      jcall = 6
      go to 70
   60 call tab(td3,td2,nsds,w(k),cd,jddm,jdd,jbit,jflag)
      jcall = 5
   70 if ( jflag .ge. 0 ) go to 80
      nsds = jdds
      go to 90
   80 if ( nsds .le. jddm ) go to 10
   90 if ( nsdm .gt. 0 ) go to 10
      nsdm = nsdso
c
c define the new value of  cd  and update  nsds .
c
      nm21 = n - m2 + 1
      cd = float(w(nm21))*pers
      if ( jcall .eq. 2 ) go to 100
      call usedin(cd,td1,jdd,nsds,loc)
      jflm = 2
      jcall = 3
      go to 110
  100 call usedin(cd,td2,jdd,nsds,loc)
      jflm = 1
      jcall = 5
  110 nsds = loc
      m = n - k
      go to 10
  120 if ( jcall .eq. 2 ) go to 130
      if ( jcall .eq. 3 ) go to 140
      if ( jcall .eq. 4 ) go to 150
      if ( jcall .eq. 5 ) go to 160
      if ( jcall .eq. 6 ) go to 170
      jflm = 1
      jfls = 1
      nsdm = nsds
      m = n - k
      cd = td1(nsds,1)
      go to 180
  130 jflm = 2
      jfls = 2
      nsdm = nsds
      m = n - k
      cd = td2(nsds,1)
      go to 180
  140 jfls = 1
      cd = td1(nsds,1)
      go to 180
  150 jfls = 3
      cd = td3(nsds,1)
      go to 180
  160 jfls = 2
      cd = td2(nsds,1)
      go to 180
  170 jfls = 3
      cd = td3(nsds,1)
  180 return
      end
      subroutine dmind(n,m,p,w,mind,jdimr,jdimc,ind,pwv)

c*********************************************************************72
c
cc DMIND defines pointers to the sorted items for 0-1 single knapsack problems.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),mind(jdimr,jdimc)
      integer ind(jdimc)
      real    pwv(jdimc)

      do i=1,m
        do j=1,n
          pwv(j) = float(p(i,j))/float(w(i,j))
          ind(j) = j
        end do
        call sortr(n,pwv,ind,jdimc)
        do j=1,n
          mind(i,j) = ind(j)
        end do
      end do

      return
      end
      subroutine enumer(n,w,c,xstar,z,lbstar,back,
     &                  x,r,wa,wb,kfix,fixit,xred,ls,lsb,local,xheu,
     &                  res,rel,jdim)

c*********************************************************************72
c
cc ENUMER performs a branch-and-bound search.
c
c computation of lower bounds l2 and l3 at all nodes.
c reduction at all nodes (but initial reduction before calling).
c heuristic algorithms at all nodes.
c dominance tests at all nodes.
c
c kfix(k)  = 0 if no item was fixed at level  k ,
c          = pointer to the first item fixed at level  k , otherwise.
c fixit(j) = 0  if item  j  is not fixed by reduction,
c          = pointer to the next item fixed by reduction at the same
c            level, otherwise ( = - 1  for the last item fixed by
c            reduction at a level).
c ls(i)    = pointer to the last item inserted in bin  i  if
c            lsb(i) = n + 1 ,
c          = pointer to the last item which can be (and was) inserted
c            with item  lsb(i)  as last but one, if lsb(i) .le. n .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim),c,xstar(jdim),z,back
      integer x(jdim),r(jdim),wa(jdim),wb(jdim),kfix(jdim),fixit(jdim),
     &        xred(jdim),ls(jdim),lsb(jdim),local(jdim),xheu(jdim),
     &        res(jdim),rel(jdim)
      integer zz
c
c initialization.
c
      mback = back
      back = 0
      jdirk = 1
c
c first heuristic.
c
      call ffdls(n,w,c,zz,r,x,ls,lsb,jdim)
      if ( zz .ge. z ) go to 20
      do i=1,n
        xstar(i) = x(i)
      end do
      z = zz
   20 do i=1,n
        fixit(i) = 0
        kfix(i) = 0
      end do
      lastw = w(n)
c
c lower bound l1.
c
      isumr = 0
      do j=1,zz
        isumr = isumr + r(j)
      end do
      isum = zz*c - isumr
      lb1 = (isum - 1)/c + 1
      if ( lb1 .gt. lbstar ) lbstar = lb1
      if ( lbstar .eq. z ) return
c
c improved lower bound.
c
      iss = 0
      do i=1,n
        if ( w(i) + w(n) .le. c ) go to 60
        iss = iss + w(i)
      end do
c
c no exit this way.
c
   60 iss = isum - iss
      lb1i = i - 1 + (iss - 1)/c + 1
      if ( lb1i .gt. lbstar ) lbstar = lb1i
      if ( lbstar .eq. z ) return
c
c lower bound l2.
c
      call l2(n,w,c,lb2,jdim)
      if ( lb2 .gt. lbstar ) lbstar = lb2
      if ( lbstar .eq. z ) return
      kzffd = zz
c
c second heuristc.
c
      mw = 1
      mb = 1
      wa(1) = c
      call hbfds(n,w,c,mb,wa,local,wb,jdim)
      if ( mb .ge. z ) go to 80
      do i=1,n
        xstar(i) = wb(i)
      end do
      z = mb
      if ( z .eq. lbstar ) return
c
c third heuristic.
c
   80 call mwfds(n,w,c,mw,wa,local,lbstar,wb,jdim)
      if ( mw .ge. z ) go to 100
      do i=1,n
        xstar(i) = wb(i)
      end do
      z = mw
      if ( z .eq. lbstar ) return
c
c iterative part.
c
c backtracking when the optimal solution has been updated.
c find the first item  k  inserted in bin  z .
c
  100 k = n
      zz = kzffd - 1
  110 if ( fixit(k) .ne. 0 ) go to 130
        j = x(k)
        r(j) = r(j) + w(k)
        if ( kfix(k) .eq. 0 ) go to 120
        call restor(k,zz,n,c,kfix,fixit,w,x,r,lastw,jdim)
  120   if ( r(kzffd) .eq. c ) go to 140
  130   k = k - 1
        jdirk = - 1
      go to 110
c
c find the next item  k  not inserted in bin  z - 1 .
c
  140 k = k - 1
        jdirk = - 1
        if ( fixit(k) .ne. 0 ) go to 140
        j = x(k)
        if ( j .lt. zz ) go to 150
        r(j) = r(j) + w(k)
        if ( kfix(k) .eq. 0 ) go to 140
        call restor(k,zz,n,c,kfix,fixit,w,x,r,lastw,jdim)
      go to 140
  150 if ( r(zz) .eq. c ) zz = zz - 1
c
c backtracking on item  k.
c
  160 if ( k .eq. 1 ) return
        if ( fixit(k) .ne. 0 ) go to 180
        if ( back .eq. mback ) return
        back = back + 1
        j = x(k)
        r(j) = r(j) + w(k)
        if ( kfix(k) .eq. 0 ) go to 170
        call restor(k,zz,n,c,kfix,fixit,w,x,r,lastw,jdim)
        if ( r(j) .lt. c ) go to 190
        go to 180
  170   if ( r(zz) .lt. c ) go to 190
        lsb(zz) = n + 1
        zz = zz - 1
  180   k = k - 1
        jdirk = - 1
        go to 160
c
c find the first bin following bin  j  where item  k  can be inserted.
c
  190   if ( j .lt. z - 1 ) go to 200
        k = k - 1
        jdirk = - 1
      go to 160
  200 j = j + 1
      if ( r(j) .lt. w(k) ) go to 190
c
c dominance tests.
c
      if ( lsb(j) .gt. n ) go to 250
      if ( j .gt. zz ) go to 250
      lsj = ls(j)
      if ( w(k) .gt. w(lsj) ) go to 210
      if ( w(k) + lastw .le. r(j) ) go to 250
      go to 190
  210 lsj = lsb(j)
      if ( w(k) .gt. w(lsj) ) go to 250
      next = ls(j) - 1
      lsj = ls(j)
  220 if ( next .le. k ) go to 190
        if ( fixit(next) .gt. 0 ) go to 230
        if ( w(next) .gt. w(lsj) ) go to 240
  230   next = next - 1
      go to 220
  240 if ( w(k) + w(next) .gt. r(j) ) go to 190
  250 x(k) = j
      r(j) = r(j) - w(k)
      if ( r(j) .lt. lastw ) go to 260
      ls(j) = k
      lsb(j) = n + 1
      go to 270
  260 lsb(j) = ls(j)
      ls(j) = k
  270 if ( j .le. zz ) go to 280
      zz = zz + 1
c
c forward step.
c
  280 if ( k .eq. n ) go to 420
c
c computation of a local lower bound.
c on output from lcl2, llb  contains the lower bound, while  na ,
c wa  and  wb  define the problem to be reduced:
c na  items with weights in  wa . from  wa(1)  to  wa(zz)  weights
c correspond to bins partially filled, from  wa(zz+1)  to  wa(na)
c correspond to free items. from  wb(1)  to  wb(zz)  pointers to the
c bins, from  wb(zz+1)  to  wb(na)  pointers to the items.
c
      call lcl2(n,w,c,isum,r,fixit,zz,z,k,na,wa,wb,llb,jdim)
      if ( llb .ge. z ) go to 160
c
c reduction.
c on return from  l3 : nbin  bins (in total, old + new), xred
c gives the corresponding partial solution, nfree the number of
c remaining free items. if nfree .lt. 0 , l3  tried to match
c pairs of bins.
c
      call l3(na,wa,c,zz,nbin,local,xred,nfree,lbr,z,xheu,isum,mr,
     &        res,rel,jdim)
      if ( nfree .lt. 0 ) go to 160
      if ( nfree .eq. 0 ) go to 330
      if ( lbr .ge. z ) go to 160
      if ( mr .ge. z ) go to 320
      z = mr
      do 290 i=1,n
        xstar(i) = x(i)
  290 continue
      jz1 = zz + 1
      do 310 ii=jz1,na
        i = wb(ii)
        j = xheu(ii)
        if ( j .le. zz ) go to 300
        xstar(i) = j
        go to 310
  300   xstar(i) = wb(j)
  310 continue
      if ( z .eq. lbstar ) return
      if ( lbr .ge. z ) go to 160
  320 call fixred(w,r,na,wa,wb,zz,x,nbin,xred,k,kfix,fixit,jdim)
      lastw = wa(na)
      go to 370
  330 if ( nbin .ge. z ) go to 160
      z = nbin
      do 340 i=1,n
        xstar(i) = x(i)
  340 continue
      jz1 = zz + 1
      do 360 ii=jz1,na
        i = wb(ii)
        j = xred(ii)
        if ( j .le. zz ) go to 350
        xstar(i) = j
        go to 360
  350   xstar(i) = wb(j)
  360 continue
      if ( z .eq. lbstar ) return
      go to 160
c
c local heuristics.
c on output from fixred, the  na  free weights are in  wa , the
c corresponding pointers in  xred .
c
  370 mb = zz
      call hbfds(na,wa,c,mb,r,local,wb,jdim)
      if ( mb .ge. z ) go to 380
      call update(n,z,xstar,na,mb,x,wb,xred,jdim)
      if ( z .eq. lbstar ) return
      if ( llb .ge. z ) go to 160
  380 mw = zz
      call mwfds(na,wa,c,mw,r,local,llb,wb,jdim)
      if ( mw .ge. z ) go to 390
      call update(n,z,xstar,na,mw,x,wb,xred,jdim)
      if ( z .eq. lbstar ) return
      if ( llb .ge. z ) go to 160
  390 k = k + 1
      if ( jdirk .eq. 1 ) go to 410
      do 400 ii=1,zz
        if ( ls(ii) .lt. k ) go to 400
        ls(ii) = lsb(ii)
        lsb(ii) = n + 1
  400 continue
  410 if ( fixit(k) .ne. 0 ) go to 370
      j = 0
      go to 190
  420 z = zz
      do 430 i=1,n
        xstar(i) = x(i)
  430 continue
      kzffd = z
      if ( z .gt. lbstar ) go to 100
      return
      end
      subroutine feas(n,m,p,w,c,xstar,jfi,jdimr,jdimc)

c*********************************************************************72
c
cc FEAS checks for infeasibility.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),c(jdimr),xstar(jdimc)

      jfi = 0

      do j=1,n
        xstar(j) = 0
        kinf = 0
        do 10 i=1,m
          if ( w(i,j) .le. c(i) ) go to 10
          kinf = kinf + 1
          p(i,j) = 0
   10   continue
        if ( kinf .eq. m ) jfi = 1
      end do

      return
      end
      subroutine ffdls(n,w,c,m,k,x,ls,lsb,jdim)

c*********************************************************************72
c
cc FFDLS performs a first-fit decreasing heuristic and initializes LS and LSB.
c
c time complexity  o(n**2) .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim),c,k(jdim),x(jdim),ls(jdim),lsb(jdim)
      m = 1
      k(1) = c - w(1)
      x(1) = 1
      ls(1) = 1
      lsb(1) = n + 1
      do 40 i=2,n
c
c insert the next item.
c
        iwi = w(i)
        do j=1,m
          if ( iwi .le. k(j) ) go to 20
        end do
c
c initialize a new bin.
c
        m = m + 1
        k(m) = c - iwi
        x(i) = m
        ls(m) = i
        lsb(m) = n + 1
        go to 40
c
c insert the item into an old bin.
c
   20   k(j) = k(j) - iwi
        x(i) = j
        if ( k(j) .lt. w(n) ) go to 30
        ls(j) = i
        go to 40
   30   lsb(j) = ls(j)
        ls(j) = i
   40 continue

      return
      end
      subroutine fixred(w,r,na,wa,wb,zz,x,nbin,xred,k,kfix,fixit,
     &                  jdim)

c*********************************************************************72
c
cc FIXRED fixes the variables after a local reduction.
c
c current solution: zz ,  x .
c current level: k .
c on output, wa  contains the weights of the  na  free items,
c xred  the corresponding pointers.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim),r(jdim),wa(jdim),wb(jdim),x(jdim),xred(jdim),
     &        kfix(jdim),fixit(jdim)
      integer zz
c
c find the first item fixed.
c
      iz1 = zz + 1
      ia = 0
      do ii=iz1,na
        if ( xred(ii) .gt. 0 ) go to 20
        ia = ia + 1
        wa(ia) = wa(ii)
        xred(ia) = wb(ii)
      end do
      na = ia
      return
   20 i = wb(ii)
      kfix(k) = i
   30 last = i
      j = xred(ii)
      if ( j .le. zz ) go to 40
      x(i) = j
      r(j) = r(j) - w(i)
      go to 50
   40 x(i) = wb(j)
      iwbj = wb(j)
      r(iwbj) = r(iwbj) - w(i)
   50 if ( ii .eq. na ) go to 70
      ii = ii + 1
      if ( xred(ii) .gt. 0 ) go to 60
      ia = ia + 1
      wa(ia) = wa(ii)
      xred(ia) = wb(ii)
      go to 50
   60 i = wb(ii)
      fixit(last) = i
      go to 30
   70 fixit(last) = - 1
      zz = nbin
      na = ia
      return
      end
      function fmed(nf,jdim,pw,n,i1,ilast)

c*********************************************************************72
c
cc FMED computes median of the ratios of the first 2 and the last item.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer nf(jdim)
      real    pw(n)
      i2 = nf(i1)
      i3 = ilast
      if ( pw(i1) .le. pw(i2) ) go to 10
      if ( pw(i1) .le. pw(i3) ) go to 20
      fmed = pw(i2)
      if ( fmed .lt. pw(i3) ) fmed = pw(i3)
      return
   10 if ( pw(i1) .ge. pw(i3) ) go to 20
      fmed = pw(i2)
      if ( fmed .gt. pw(i3) ) fmed = pw(i3)
      return
   20 fmed = pw(i1)
      return
      end
      subroutine forwd(na,a,n6,ss,lev,l,r,t,nxt,v,jflag)

c*********************************************************************72
c
cc FORWRD performs statements 1-9.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension a(na),ss(n6)
      integer l(6),r(6),t(6)
      il = l(lev)
      ir = r(lev)
      it = t(lev)
      nn = ir - il + 1
      itarg = nn
c
c statement  1 .
c
      if ( nn .gt. 97 ) go to 20
   10 call mpsort(na,a,il,ir,it,v)
      jflag = 1
      return
c
c statement  2 .
c
   20 itarg = 59*itarg/70
      ilc = il
      imed = (il + ir)/2
      p = a(imed)
      if ( a(il) .le. p ) go to 30
      a(imed) = a(il)
      a(il) = p
      p = a(imed)
   30 irc = ir
      if ( a(ir) .ge. p ) go to 50
      a(imed) = a(ir)
      a(ir) = p
      p = a(imed)
      if ( a(il) .le. p ) go to 50
      a(imed) = a(il)
      a(il) = p
      p = a(imed)
      go to 50
   40 a(irc) = a(ilc)
      a(ilc) = aux
   50 irc = irc - 1
      if ( a(irc) .gt. p ) go to 50
      aux = a(irc)
   60 ilc = ilc + 1
      if ( a(ilc) .lt. p ) go to 60
      if ( ilc .le. irc ) go to 40
c
c statement  3 .
c
      if ( it .gt. ilc - il ) go to 70
      ir = ilc - 1
      go to 80
c
c statement  4 .
c
   70 it = it - ( ilc - il )
      il = ilc
   80 nn = ir - il + 1
      if ( nn .le. 97 ) go to 10
c
c statement  5 .
c
      if ( nn .le. itarg ) go to 20
c
c statement  6 .
c
      jflag = 0
      l(lev) = il
      r(lev) = ir
      t(lev) = it
      lev = lev + 1
      nn7 = nn/7
      ii = il + 7*(nn7 - 1)
      next = nxt
      l(lev) = nxt
      do i=il,ii,7
        call sort7(na,a,i)
        ss(next) = a(i+3)
        next = next + 1
      end do
      i1 = ii + 7
      i2 = ir - 1
  100 if ( i1 .gt. i2 ) go to 120
      do 110 i=i1,i2
        if ( a(i) .le. a(i+1) ) go to 110
        ap = a(i)
        a(i) = a(i+1)
        a(i+1) = ap
  110 continue
      i2 = i2 - 1
      go to 100
  120 r(lev) = next - 1
c
c statement  7 .
c
      it1 = it
      n1 = (11*nn + 279)/280
      it = it/7
      if ( n1 .gt. it ) it = n1
      n2 = nn7 - n1 + 1
      if ( n2 .lt. it ) it = n2
c
c statement  8 .
c
      if ( it .le. (it1 + 3)/4 ) go to 130
      t(lev) = (it1 + 3)/4
      return
c
c statement  9 .
c
  130 itt = nn7 - (nn - it1 + 4)/4 + 1
      if ( it .lt. itt ) it = itt
      t(lev) = it
      return
      end
      subroutine gha(p,w,c,n,m,z,xstar,iub,best,kvst,inf,
     &               jdimr,jdimc,kw,mw,pen,first,second,bb)

c*********************************************************************72
c
cc GHA applies the approximate algorithm gh with function (a).
c
c and
c define the infinite value  inf .
c
c if iub = z the solution is optimal;
c if z = kvst no feasible solution was found.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),c(jdimr),xstar(jdimc),
     &        best(jdimc),z
      integer kw(jdimr),mw(jdimr),pen(jdimc),first(jdimc),
     &        second(jdimc),bb(jdimc)
      integer fmax,smax
      inf = 0
      do i=1,m
        kw(i) = c(i)
        mw(i) = 0
        if ( c(i) .gt. inf ) inf = c(i)
      end do
      iub = 0
      z = 0
      kvst = 0
      do 40 j=1,n
        ipmin = p(1,j)
        fmax = p(1,j)
        if = 1
        smax = 0
        do 30 i=2,m
          if ( p(i,j) .lt. ipmin ) ipmin = p(i,j)
          if ( smax .ge. p(i,j) ) go to 30
          if ( fmax .ge. p(i,j) ) go to 20
          smax = fmax
          is = if
          fmax = p(i,j)
          if = i
          go to 30
   20     smax = p(i,j)
          is = i
   30   continue
        kvst = kvst + ipmin
        first(j) = if
        best(j) = if
        second(j) = is
        pen(j) = fmax - smax
        if ( smax .eq. 0 ) pen(j) = - 1
        bb(j) = j
        iub = iub + fmax
        if ( w(if,j) .gt. mw(if) ) mw(if) = w(if,j)
        if ( w(is,j) .gt. mw(is) ) mw(is) = w(is,j)
   40 continue
      if ( kvst .gt. 0 ) kvst = kvst - 1
      if ( iub .gt. inf ) inf = iub
      do 50 j=1,n
        if ( pen(j) .eq. (- 1) ) pen(j) = inf
   50 continue
      nb = n
   60 maxpen = - 1
      do 70 jj=1,nb
        j = bb(jj)
        if ( pen(j) .le. maxpen ) go to 70
        maxpen = pen(j)
        jjm = jj
   70 continue
      jo = bb(jjm)
      io = first(jo)
      z = z + p(io,jo)
      xstar(jo) = io
      bb(jjm) = bb(nb)
      nb = nb - 1
      if ( nb .eq. 0 ) return
      kw(io) = kw(io) - w(io,jo)
      if ( mw(io) .le. kw(io) ) go to 60
      do 120 jj=1,nb
        j = bb(jj)
        if ( w(io,j) .le. kw(io) ) go to 120
        if ( first(j) .ne. io ) go to 80
        if ( pen(j) .eq. inf ) go to 130
        first(j) = second(j)
        go to 90
   80   if ( second(j) .ne. io ) go to 120
   90   index = first(j)
        w(index,j) = w(index,j) + inf
        newsec = 0
        do 100 i=1,m
          if ( w(i,j) .gt. kw(i) ) go to 100
          if ( p(i,j) .le. newsec ) go to 100
          newsec = p(i,j)
          is = i
  100   continue
        w(index,j) = w(index,j) - inf
        if ( newsec .eq. 0 ) go to 110
        second(j) = is
        pen(j) = p(index,j) - newsec
        if ( w(is,j) .gt. mw(is) ) mw(is) = w(is,j)
        go to 120
  110   pen(j) = inf
  120 continue
      go to 60
  130 z = kvst
      return
      end
      subroutine ghbcd(p,w,c,n,m,z,xstar,inf,jdimr,jdimc,xsp,
     &           dmyr1,dmyr2,dmyr3,dmyr4,dmyr5,
     &           dmyc2,dmyc3,dmyc4,dmycr1,dmya)

c*********************************************************************72
c
cc GHBCD applies the approximate algorithm GH with functions (b), (c) and (d).
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),c(jdimr),xstar(jdimc),z
      integer vsp,xsp(jdimc)
      integer dmyr1(jdimr),dmyr2(jdimr),dmyr3(jdimr),dmyr4(jdimr),
     &        dmyr5(jdimr),dmyc2(jdimc),dmyc3(jdimc),dmyc4(jdimc),
     &        dmya(jdimr,jdimc)
      real    dmycr1(jdimc)

      jj = 2
      a1 = 1.
      a2 = 0.
      a3 = 0.
      a4 = 0.
      a5 = 1.
   10 call ghx(p,w,c,n,m,vsp,xsp,a1,a2,a3,a4,a5,inf,jdimr,jdimc,
     &         dmyr1,dmyr2,dmyr3,dmyr4,dmyr5,dmyc2,dmyc3,dmyc4,
     &         dmycr1,dmya)
      if ( vsp .le. z ) go to 30
      z = vsp
      do 20 j=1,n
         xstar(j) = xsp(j)
   20 continue
   30 if ( jj .eq. 3 ) go to 40
      if ( jj .eq. 4 ) go to 50
      jj = 3
      a1 = 1.
      a2 = 0.
      a3 = 1.
      a4 = 0.
      a5 = 0.
      go to 10
   40 jj = 4
      a1 = 0.
      a2 = 1.
      a3 = 0.
      a4 = 1.
      a5 = 0.
      go to 10
   50 return
      end
      subroutine ghx(p,w,c,n,m,z,xstar,a1,a2,a3,a4,a5,inf,jdimr,jdimc,
     &               kw,mw,minw,kchan,kwr,first,second,bb,pen,wl)

c*********************************************************************72
c
cc GHX applies the approximate algorithm gh with function (b) or (c) or (d).
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),xstar(jdimc),c(jdimr),z
      integer kw(jdimr),mw(jdimr),minw(jdimr),kchan(jdimr),kwr(jdimr),
     &        first(jdimc),second(jdimc),bb(jdimc),wl(jdimr,jdimc)
      real    pen(jdimc),maxpen

      do i=1,m
        kw(i) = c(i)
        mw(i) = 0
        minw(i) = inf
        do j=1,n
          wl(i,j) = w(i,j)
          if ( wl(i,j) .lt. minw(i) ) minw(i) = wl(i,j)
        end do
        kwr(i) = kw(i) - minw(i)
      end do

      z = 0
      do 60 j=1,n
        fmax = - inf
        if = 0
        smax = - inf
        do 40 i=1,m
          if ( wl(i,j) .gt. kw(i) ) go to 40
          if ( wl(i,j) .gt. kwr(i) ) wl(i,j) = kw(i)
          rwl = wl(i,j)
          rp = p(i,j)
          rkw = kw(i)
          s = (- a1*rwl + a2*rp)/(a3*rkw + a4*rwl + a5)
          if ( smax .ge. s ) go to 40
          if ( fmax .ge. s ) go to 30
          smax = fmax
          is = if
          fmax = s
          if = i
          go to 40
   30     smax = s
          is = i
   40   continue
        first(j) = if
        second(j) = is
        pen(j) = fmax - smax
        bb(j) = j
        if ( wl(if,j) .gt. mw(if) ) mw(if) = wl(if,j)
        if ( smax .gt. float(- inf) ) go to 50
        pen(j) = inf
        go to 60
   50   if ( wl(is,j) .gt. mw(is) ) mw(is) = wl(is,j)
   60 continue
      nb = n
   70 maxpen = - 1
      do 80 jj=1,nb
        j = bb(jj)
        if ( pen(j) .le. maxpen ) go to 80
        maxpen = pen(j)
        jjm = jj
   80 continue
      jo = bb(jjm)
      io = first(jo)
      z = z + p(io,jo)
      xstar(jo) = io
      bb(jjm) = bb(nb)
      nb = nb - 1
      kw(io) = kw(io) - w(io,jo)
      if ( nb .eq. 0 ) go to 210
      kk = 0
      do 110 i=1,m
        kchan(i) = 0
        if ( wl(i,jo) .gt. minw(i) ) go to 100
        minw(i) = inf
        do 90 jj=1,nb
          j = bb(jj)
          if ( wl(i,j) .lt. minw(i) ) minw(i) = wl(i,j)
   90   continue
        if ( minw(i) + mw(i) .le. kw(i) ) go to 100
        kk = 1
        kchan(i) = 1
  100   kwr(i) = kw(i) - minw(i)
  110 continue
      if ( mw(io) .le. kw(io) ) go to 120
      kk = 1
      kchan(io) = 1
  120 if ( kk .eq. 0 ) go to 70
      do 190 jj=1,nb
        j = bb(jj)
        jf = first(j)
        if ( pen(j) .lt. float(inf) ) go to 130
        if ( wl(jf,j) .gt. kw(jf) ) go to 200
        go to 190
  130   if ( kchan(jf) .eq. 0 ) go to 140
        if ( wl(jf,j) .gt. kwr(jf) ) go to 150
  140   js = second(j)
        if ( kchan(js) .eq. 0 ) go to 190
        if ( wl(js,j) .le. kwr(js) ) go to 190
  150   fmax = - inf
        smax = - inf
        if = 0
        do 170 i=1,m
          if ( wl(i,j) .gt. kw(i) ) go to 170
          if ( wl(i,j) .gt. kwr(i) ) wl(i,j) = kw(i)
          rwl = wl(i,j)
          rp = p(i,j)
          rkw = kw(i)
          s = (- a1*rwl + a2*rp)/(a3*rkw + a4*rwl + a5)
          if ( smax .ge. s ) go to 170
          if ( fmax .ge. s ) go to 160
          smax = fmax
          is = if
          fmax = s
          if = i
          go to 170
  160     smax = s
          is = i
  170   continue
        first(j) = if
        second(j) = is
        pen(j) = fmax - smax
        if ( wl(if,j) .gt. mw(if) ) mw(if) = wl(if,j)
        if ( smax .gt. float(- inf) ) go to 180
        pen(j) = inf
        go to 190
  180   if ( wl(is,j) .gt. mw(is) ) mw(is) = wl(is,j)
  190 continue
      go to 70
  200 z = 0
      return
c
c try to improve on the current solution z.
c
  210 do 230 j=1,n
        if = xstar(j)
        maxp = p(if,j)
        do 220 i=1,m
          if ( w(i,j) .gt. kw(i) ) go to 220
          if ( p(i,j) .le. maxp ) go to 220
          maxp = p(i,j)
          if = i
  220   continue
        ip = xstar(j)
        if ( if .eq. ip ) go to 230
        xstar(j) = if
        z = z + p(if,j) - p(ip,j)
        kw(ip) = kw(ip) + w(ip,j)
        kw(if) = kw(if) - w(if,j)
  230 continue

      return
      end
      subroutine gr1(p,w,c,n,m,z,xstar,iub,best,b,a,nr,kq,kvst,
     &               jdimr,jdimc)

c*********************************************************************72
c
cc GR1 reduces a maximization gap.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),c(jdimr),xstar(jdimc),
     &        best(jdimc),b(jdimc),a(jdimr,jdimc),kq(jdimr),z
      integer po,bej

      nr = n
      do i=1,m
        kq(i) = c(i)
      end do

      do j=1,n
        b(j) = 1
        do i=1,m
          a(i,j) = 0
        end do
      end do

      if ( z .eq. kvst ) return
      m1 = m - 1
      jgap = iub - z
   40 nvr = n*m
      nrold = nr
      do 90 j=1,n
        if ( b(j) .eq. 1 ) go to 50
        nvr = nvr - m
        go to 90
   50   bej = best(j)
        po = p(bej,j)
        nf = 0
        do 80 i=1,m
          if ( w(i,j) .gt. kq(i) ) go to 60
          if ( po - p(i,j) .lt. jgap ) go to 70
   60     a(i,j) = - 1
          nf = nf + 1
          go to 80
   70     ib = i
   80   continue
        if ( nf .ge. m ) go to 100
        nvr = nvr - nf
        if ( nf .lt. m1 ) go to 90
        b(j) = 0
        a(ib,j) = 1
        kq(ib) = kq(ib) - w(ib,j)
        nr = nr - 1
        nvr = nvr - 1
   90 continue
      if ( nr .eq. 0 ) return
      if ( nr .lt. nrold ) go to 40
      return
  100 nr = 0
      nvr = 0
      return
      end
      subroutine gr2(n,m,p,w,q,b,a,mind,pak,kap,pakl,ip,ir,il,if,nr,
     &               z,xstar,jub,x,v,flrep,kvst,jdimr,jdimc,jnlev,in)

c*********************************************************************72
c
cc GR2 reduces a maximization gap.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),q(jdimr),b(jdimc),
     &        a(jdimr,jdimc),mind(jdimr,jdimc),pak(jdimr,jdimc),
     &        kap(jdimr,jdimc),pakl(jdimr),ip(jdimr),ir(jdimr),
     &        il(jdimr),if(jdimr),xstar(jdimc),x(jdimr,jdimc),
     &        v(jnlev,jdimr),flrep(jdimr),z
      integer in(jdimr)
      do i=1,m
        flrep(i) = 0
      end do

      if ( z .eq. kvst ) return
      call prepen(n,m,p,w,q,b,a,mind,pak,kap,pakl,ip,ir,il,if,
     &            jdimr,jdimc)
      jgap = jub - z
      nvr = 0
      ipnr0 = 0
      do 210 j=1,n
        if ( b(j) .eq. 0 ) go to 210
        n1 = 0
        na = 0
        isum = 0
        do 140 i=1,m
          if ( a(i,j) .eq. (- 1) ) go to 140
          if ( w(i,j) .gt. q(i) ) go to 120
          if ( x(i,j) .eq. 0 ) go to 90
c
c compute the loss lam of knapsack i if x(i,j) = 0 .
c
          jj = kap(i,j)
          kl = il(i)
          kp = ip(i)
          kr = ir(i)
          if ( jj - (kl + 1) ) 40,20,130
   20     if ( kl + 1 .eq. pakl(i) ) go to 30
          jkl = pak(i,kl+2)
          rub = float(kp) + float(p(i,jkl)*kr)/float(w(i,jkl))
          go to 80
   30     rub = kp
          go to 80
   40     kp = kp - p(i,j)
          kr = kr + w(i,j)
          la = kl + 1
          l2 = pakl(i)
          if ( la .le. l2 ) go to 50
          rub = kp
          go to 80
   50     do 60 jl=la,l2
            jj = pak(i,jl)
            if ( w(i,jj) .gt. kr ) go to 70
            kr = kr - w(i,jj)
            kp = kp + p(i,jj)
   60     continue
          rub = kp
          go to 80
   70     rub = float(kp) + float(p(i,jj)*kr)/float(w(i,jj))
   80     iub = rub
          lam = v( 1,i) - (iub + if(i))
          if ( lam .ge. jgap ) go to 150
          if ( lam .le. 0 ) go to 130
          isum = isum + lam
          n1 = n1 + 1
          in(n1) = i
          if ( isum .ge. jgap ) go to 170
          go to 130
c
c compute the loss lam of knapsack i if x(i,j) = 1 .
c
   90     jj = kap(i,j)
          kl = il(i)
          if ( jj .le. kl ) go to 130
          kr = ir(i) - w(i,j)
          kp = ip(i) + p(i,j)
  100     if ( kr .ge. 0 ) go to 110
          jkl = pak(i,kl)
          kr = kr + w(i,jkl)
          kp = kp - p(i,jkl)
          kl = kl - 1
          go to 100
  110     jkl = pak(i,kl+1)
          rub = float(kp) + float(p(i,jkl)*kr)/float(w(i,jkl))
          iub = rub
          lam = v(1,i) - (iub + if(i))
          if ( lam .lt. jgap ) go to 130
          a(i,j) = - 1
          flrep(i) = 1
          go to 140
  120     a(i,j) = - 1
          flrep(i) = 1
          go to 140
  130     na = na + 1
          ib = i
  140   continue
        if ( na .eq. 0 ) go to 310
        if ( na .gt. 1 ) go to 200
        i = ib
  150   b(j) = 0
        do ii=1,m
          a(ii,j) = - 1
          flrep(ii) = 1
        end do
        a(i,j) = 1
        nr = nr - 1
        q(i) = q(i) - w(i,j)
        ipnr0 = ipnr0 + p(i,j)
        go to 210

  170   do i=1,m
          if ( a(i,j) .ne. (- 1) ) then
            a(i,j) = - 1
            flrep(i) = flrep(i) + 1
          end if
        end do

        do ii=1,n1
          i = in(ii)
          a(i,j) = 0
          flrep(i) = flrep(i) - 1
        end do

        na = n1
  200   nvr = nvr + na
  210 continue
      do i=1,m
        if ( flrep(i) .gt. 1 ) flrep(i) = 1
      end do
      if ( nr .gt. 1 ) return
      if ( nr .eq. 0 ) go to 260
      max = - 1
      do j=1,n
        if ( b(j) .eq. 1 ) go to 240
      end do

  240 b(j) = 0
      nr = 0
      do 250 i=1,m
        if ( a(i,j) .eq. (- 1) ) go to 250
        if ( w(i,j) .gt. q(i) ) go to 250
        if ( p(i,j) .le. max ) go to 250
        max = p(i,j)
        ii = i
  250 continue
      if ( max .lt. 0 ) return
      a(ii,j) = 1
      q(ii) = q(ii) - w(ii,j)
      ipnr0 = ipnr0 + p(ii,j)
  260 do i=1,m
        ipnr0 = ipnr0 + if(i)
      end do

      if ( ipnr0 .le. z ) return
      z = ipnr0
      do 300 j=1,n
        do i=1,m
          if ( a(i,j) .eq. 1 ) go to 290
        end do
  290   xstar(j) = i
  300 continue
      return
  310 nr = 0
      return
      end
      subroutine hbfds(n,w,c,m,kk,k,x,jdim)

c*********************************************************************72
c
cc HBFDS performs a best-fit decreasing heuristic.
c
c for local use with current solution given.
c time complexity  o(n**2) .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim),c,k(jdim),x(jdim),kk(jdim)
      do j=1,m
        k(j) = kk(j)
      end do

      do 40 i=1,n
c
c insert the next item.
c
        iwi = w(i)
        minres = c
        do 20 j=1,m
          kres = k(j) - iwi
          if ( kres .lt. 0 ) go to 20
          if ( kres .ge. minres ) go to 20
          minres = kres
          jm = j
   20   continue
        if ( minres . lt. c ) go to 30
c
c initialize a new bin.
c
        m = m + 1
        k(m) = c - iwi
        x(i) = m
        go to 40
c
c insert the item  into an old bin.
c
   30   k(jm) = k(jm) - iwi
        x(i) = jm
   40 continue

      return
      end
      subroutine heur(p,w,c,n,m,z,xstar,iub,jub,best,kvst,inf,
     &                jdimr,jdimc,dmyr1,dmyr2,dmyr3,dmyr4,dmyr5,
     &                dmyc1,dmyc2,dmyc3,dmyc4,dmycr1,a)

c*********************************************************************72
c
cc HEUR determines the best initial heuristic solution.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),c(jdimr),z,xstar(jdimc),
     &        best(jdimc)
      integer dmyr1(jdimr),dmyr2(jdimr),dmyr3(jdimr),dmyr4(jdimr),
     &        dmyr5(jdimr)
      integer dmyc1(jdimc),dmyc2(jdimc),dmyc3(jdimc),dmyc4(jdimc)
      real    dmycr1(jdimc)
      integer a(jdimr,jdimc)
c
c first heuristic solution.
c
      call gha(p,w,c,n,m,z,xstar,iub,best,kvst,inf,
     &         jdimr,jdimc,dmyr1,dmyr2,dmyc1,dmyc2,dmyc3,dmyc4)
      jub = iub
      if ( z .eq. jub ) return
c
c second heuristic solution.
c
      call ghbcd(p,w,c,n,m,z,xstar,inf,
     &           jdimr,jdimc,dmyc1,dmyr1,dmyr2,dmyr3,dmyr4,dmyr5,
     &           dmyc2,dmyc3,dmyc4,dmycr1,a)
      return
      end
      subroutine impr1(n,p,w,m,z,x,cr,inf,jdn,jdm,f)

c*********************************************************************72
c
cc IMPR1: first improvement.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdn),w(jdn),x(jdn),cr(jdm),z
      integer f(jdm,jdm),cp,wp,ff,u,t,q,r,s,d
      mp1 = m + 1
      cr(mp1) = 0
      maxf = 0
      cp = 0

      do i=1,m
        ip1 = i + 1
        do 10 j=ip1,mp1
          f(i,j) = cr(i) + cr(j)
          f(j,i) = f(i,j)
          if ( f(i,j) .le. maxf ) go to 10
          maxf = f(i,j)
          ip = i
          jp = j
   10   continue
        f(i,i) = 0
        if ( cp .lt. cr(i) ) cp = cr(i)
      end do

      f(mp1,mp1) = 0
      do 30 j=1,n
        if ( x(j) .lt. mp1 ) go to 30
        ff = j
        go to 40
   30 continue
      return
   40 wp = w(ff)
      if ( ff .eq. n ) go to 60
      if1 = ff + 1
      do 50 j=if1,n
        if ( x(j) .lt. mp1 ) go to 50
        if ( w(j) .lt. wp ) wp = w(j)
   50 continue
   60 if ( f(ip,jp) .lt. wp ) return
      j = 1
   70 ixj = x(j)
      if ( cr(ixj) + cp .lt. wp ) go to 230
      k = j + 1
   80 if ( k .gt. n ) go to 230
      ixj = x(j)
      ixk = x(k)
      if ( f(ixj,ixk) .lt. wp ) go to 120
      if ( w(j) - w(k) ) 90,120,100
   90 u = k
      t = j
      go to 110
  100 u = j
      t = k
  110 d = w(u) - w(t)
      i = x(u)
      ixt = x(t)
      if ( d .gt. cr(ixt) )go to 120
      if ( cr(i) + d .ge. wp ) go to 130
  120 k = k + 1
      go to 80
  130 icipd = cr(i) + d
      maxp = 0
      do 140 q=ff,n
        if ( x(q) .lt. mp1 ) go to 140
        if ( w(q) .gt. icipd ) go to 140
        if ( p(q) .le. maxp ) go to 140
        r = q
        maxp = p(r)
  140 continue
      cr(i) = cr(i) + d - w(r)
      cr(ixt) = cr(ixt) - d
      z = z + p(r)
      do 150 q=1,m
        f(i,q) = cr(i) + cr(q)
        f(q,i) = f(i,q)
        f(ixt,q) = cr(ixt) + cr(q)
        f(q,ixt) = f(ixt,q)
  150 continue
      f(i,i) = 0
      f(ixt,ixt) = 0
      if ( i .eq. ip ) go to 160
      if ( i .eq. jp ) go to 160
      if ( ixt .eq. ip ) go to 160
      if ( ixt .ne. jp ) go to 190
  160 maxf = 0
      do 180 q=1,m
        ip1 = q + 1
        do 170 s=ip1,mp1
          if ( f(q,s) .le. maxf ) go to 170
          maxf = f(q,s)
          ip = q
          jp = s
  170   continue
  180 continue
  190 x(r) = i
      x(u) = ixt
      x(t) = i
      if ( w(r) .ne. wp ) go to 210
      wp = inf
      do 200 s=ff,n
        if ( x(s) .lt. mp1 ) go to 200
        if ( w(s) .lt. wp ) wp = w(s)
  200 continue
  210 if ( f(ip,jp) .lt. wp ) return
      cp = 0
      do 220 s=1,m
        if ( cp .lt. cr(s) ) cp = cr(s)
  220 continue
      ixj = x(j)
      if ( cr(ixj) + cp .lt. wp ) go to 230
      k = k + 1
      go to 80
  230 if ( j .eq. n ) return
      j = j + 1
      go to 70
      end
      subroutine impr2(n,p,w,m,z,x,cr,min,xx,inf,jdn,jdm)

c*********************************************************************72
c
cc IMPR2: second improvement.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdn),w(jdn),x(jdn),min(jdn),xx(jdn),cr(jdm),z
      integer f,t,v,cb,u,s
      mp1 = m + 1
      mink = inf
      min(n) = mink

      do i=2,n
        kk = n + 2 - i
        if ( w(kk) .lt. mink ) mink = w(kk)
        min(kk-1) = mink
      end do

      do 20 j=1,n
        if ( x(j) .le. m ) go to 20
        f = j
        go to 30
   20 continue
      return
   30 s = n
      j = n
   40 if ( x(j) .eq. mp1 ) go to 140
      ixj = x(j)
      cb = cr(ixj) + w(j)
      if ( cb*p(f)/w(f) .le. p(j) ) go to 140
      if ( cb .ge. w(f) ) go to 50
      if ( cb .lt. min(f) ) go to 140
   50 k = f
      t = 0
      v = 0
   60 if ( w(k) .gt. cb ) go to 70
      v = v + p(k)
      cb = cb - w(k)
      t = t + 1
      xx(t) = k
      if ( cb .lt. min(k) ) go to 100
   70 if ( k .eq. n ) go to 100
      k1 = k + 1
      do 80 u=k1,n
        if ( x(u) .le. m ) go to 80
        k = u
        go to 90
   80 continue
      go to 100
   90 if ( v + cb*p(k)/w(k) .gt. p(j) ) go to 60
  100 if ( v .le. p(j) ) go to 140
      s = j
      ixj = x(j)
      cr(ixj) = cb
      do 110  k=1,t
        ixxk = xx(k)
        x(ixxk) = x(j)
  110 continue
      x(j) = mp1
      z = z + v - p(j)
      if ( j .gt. f ) go to 120
      f = j
      go to 140
  120 if ( x(f) .eq. mp1 ) go to 140
      if1 = f + 1
      do 130 u=if1,n
        if ( x(u) .le. m ) go to 130
        f = u
        go to 140
  130 continue
  140 j = j - 1
      if ( j .eq. 0 ) j = n
      if ( j .eq. s ) return
      go to  40
      end
      subroutine insert(i,m,fs,x,ifp,k,jdim)

c*********************************************************************72
c
cc INSERT inserts item i in bin m and updates fs, x, ifp, k.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer x(jdim),k(jdim),fs
      is = x(i)
      ip = k(i)
      if ( is .gt. 0 ) go to 10
      ifp = ip
      go to 20
   10 k(is) = ip
   20 if ( ip .gt. 0 ) go to 30
      fs = is
      go to 40
   30 x(ip) = is
   40 x(i) = m
      return
      end
      subroutine knapsack_reorder ( n, p, w )

c*********************************************************************72
c
cc KNAPSACK_REORDER reorders knapsack data by "profit density".
c
c  Discussion:
c
c    This routine must be called to rearrange the data before calling
c    routines that handle a knapsack problem.
c
c    The "profit density" for object I is defined as P(I)/W(I).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Combinatorial Algorithms,
c    CRC Press, 1998,
c    ISBN: 0-8493-3988-X,
c    LC: QA164.K73.
c
c  Parameters:
c
c    Input, integer N, the number of objects.
c
c    Input/output, integer P(N), the "profit" or value of each object.
c
c    Input/output, integer W(N), the "weight" or cost of each object.
c
      implicit none

      integer n

      integer i
      integer j
      integer p(n)
      integer t
      integer w(n)
c
c  Rearrange the objects in order of "profit density".
c
      do i = 1, n
        do j = i+1, n
          if ( p(i) * w(j) < p(j) * w(i) ) then
            t    = p(i)
            p(i) = p(j)
            p(j) = t
            t    = w(i)
            w(i) = w(j)
            w(j) = t
          end if
        end do
      end do

      return
      end
      subroutine kp01m(n,p,w,c,minw0,z,x,jfo,iubf0,np1,
     &                 xx,ps,ws,zs,minw)

c*********************************************************************72
c
cc KP01M solves, through branch-and-bound, a 0-1 single knapsack problem.
c
c  Discussion:
c
c    it is assumed that the items are sorted according to decreasing
c    profit per unit weight.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(np1),w(np1),x(n),c,z
      integer xx(n),ps(n),ws(n),zs(n)
      real    minw(n)
      integer cwf,cws,diff,val,r,t,ub,ub1
c
c initialize.
c
      cwf = c
      ip = 0
      cws = c

      do ll=1,n
        if ( w(ll) .gt. cws ) go to 20
        ip = ip + p(ll)
        cws = cws - w(ll)
      end do

   20 ll = ll - 1
      ub = 0
      if ( cws .eq. 0 ) go to 80
      p(np1) = 0
      w(np1) = c + 1
      ntest = 2*n
      if ( iubf0 .lt. 0 ) go to 30
      knp1 = (c + 1)/w(n) + 1
      p(np1) = p(n)*knp1
      w(np1) = w(n)*knp1
      ntest = n
c
c ntest is used to avoid integer overflows when iubf0 = 0 .
c
   30 if ( ll + 2 .gt. ntest ) go to 40
      ub = ip + cws*p(ll+2)/w(ll+2)
      go to 50
   40 ub = ip + cws*p(n)/w(n)
   50 a = w(ll+1) - cws
      ub1 = float(ip + p(ll+1)) - a*float(p(ll))/float(w(ll))
      if ( ub1 .gt. ub ) ub = ub1
      if ( iubf0 .eq. 0 ) iubf0 = ub
      if ( jfo .eq. 0 .and. n .gt. 10 ) go to 80
      mink = c + 1
      minw(n) = mink
      do 60 j=2,n
        kk = n + 2 - j
        if ( w(kk) .lt. mink ) mink = w(kk)
        minw(kk-1) = mink
   60 continue
      do 70 j=1,n
        xx(j) = 0
   70 continue
      val = 0
      lold = n
      ii = 1
      if ( iubf0 .lt. 0 ) go to 220
      iubf0 = 0
      fw1 = w(1)
      fp1 = p(1)
      fpn1 = float(minw0)*float(p(n))/float(w(n))
      a = w(ll+1) - cws
      ib = float(ip + p(ll+1)) - a*fp1/fw1
      if ( ib .gt. iubf0 ) iubf0 = ib
      go to 220
   80 z = ip
      nn = ll + 1
      do 90 j=nn,n
        x(j) = - x(j)
   90 continue
      go to 580
c
c try to insert the ii-th item into the current solution.
c
  100 if ( w(ii) .le. c ) go to 120
      if ( iubf0 .lt. 0 ) go to 110
      a = w(ii) - c
      ib = float(val + p(ii)) - a*fp1/fw1
      if ( ib .gt. iubf0 ) iubf0 = ib
  110 ii1 = ii + 1
      if ( z .ge. c*p(ii1)/w(ii1) + val ) go to 360
      ii = ii1
      go to 100
c
c build a new current solution.
c
  120 ip = ps(ii)
      cws = c - ws(ii)
      in = zs(ii)
      do 130 ll=in,n
        if ( w(ll) .gt. cws ) go to 200
        ip = ip + p(ll)
        cws = cws - w(ll)
  130 continue
      ll = n
      if ( iubf0 .lt. 0 ) go to 140
      call newb(cws,ip+val,minw0,p(n),w(n),fp1,fpn1,fw1,iubf0)
      call newb(cws+w(n),ip+val-p(n),minw0,p(n),w(n),fp1,fpn1,
     &          fw1,iubf0)
  140 if ( z .ge. ip + val ) go to 360
      z = ip + val
      nn = ii - 1
      do 160 j=1,nn
        if ( xx(j) .eq. 0 ) go to 150
        x(j) = iabs(x(j))
        go to 160
  150   x(j) = - iabs(x(j))
  160 continue
      do 170 j=ii,ll
        x(j) = iabs(x(j))
  170 continue
      if ( ll .eq. n ) go to 190
      nn = ll + 1
      do 180 j=nn,n
        x(j) = - iabs(x(j))
  180 continue
  190 if ( z .ne. ub ) go to 360
      c = cwf
      go to 580
  200 ll = ll - 1
      if ( iubf0 .lt. 0 ) go to 210
      a = w(ll+1) - cws
      ib = float(val + ip + p(ll+1)) - a*fp1/fw1
      if ( ib .gt. iubf0 ) iubf0 = ib
  210 if ( cws .eq. 0 ) go to 140
      if ( z .ge. val + ip + cws*p(ll+1)/w(ll+1) ) go to 360
c
c save the current solution.
c
  220 ws(ii) = c - cws
      ps(ii) = ip
      zs(ii) = ll + 1
      xx(ii) = 1
      nn = ll - 1
      if ( nn .lt. ii ) go to 240
      do 230 j=ii,nn
        jp1 = j + 1
        ws(jp1) = ws(j) - w(j)
        ps(jp1) = ps(j) - p(j)
        zs(jp1) = ll + 1
        xx(jp1) = 1
  230 continue
  240 j1 = ll + 1
      do 250 j=j1,lold
        ws(j) = 0
        ps(j) = 0
        zs(j) = j
  250 continue
      lold = ll
      c = cws
      val = val + ip
      if ( ll - (n - 2) ) 300, 270, 260
  260 ii = n
      if ( iubf0 .ge. 0 ) go to 290
      go to 320
  270 if ( c .lt. w(n) ) go to 280
      c = c - w(n)
      val = val + p(n)
      xx(n) = 1
      if ( iubf0 .lt. 0 ) go to 280
      ii = n - 1
      call newb(c+w(n),val-p(n),minw0,p(n),w(n),fp1,fpn1,fw1,
     &          iubf0)
      go to 290
  280 ii = n - 1
      if ( iubf0 .lt. 0 ) go to 320
      a = w(n) - c
      ib = float(val + p(n)) - a*fp1/fw1
      if ( ib .gt. iubf0 ) iubf0 = ib
  290 call newb(c,val,minw0,p(n),w(n),fp1,fpn1,fw1,iubf0)
      go to 320
  300 ii = ll + 2
      if ( c .ge. int(minw(ii-1)) ) go to 100
      if ( iubf0 .lt. 0 ) go to 320
c
c compute the bound corresponding to the insertion of items
c ii,...,n.
c
      do 310 j=ii,n
        if ( z .ge. val + c*p(j)/w(j) ) go to 320
        a = w(j) - c
        ib = float(val + p(j)) - a*fp1/fw1
        if ( ib .gt. iubf0 ) iubf0 = ib
  310 continue
      call newb(c,val,minw0,p(n),w(n),fp1,fpn1,fw1,iubf0)
c
c save the current optimal solution.
c
  320 if ( z .ge. val ) go to 350
      z = val
      do 340 j=1,n
        if ( xx(j) .eq. 0 ) go to 330
        x(j) = iabs(x(j))
        go to 340
  330   x(j) = - iabs(x(j))
  340 continue
      if ( z .ne. ub ) go to 350
      c = cwf
      go to 580
  350 if ( xx(n) .eq. 0 ) go to 360
      xx(n) = 0
      c = c + w(n)
      val = val - p(n)
c
c backtrack.
c
  360 nn = ii - 1
      if ( nn .eq. 0 ) go to 380
      do 370 j=1,nn
        kk = ii - j
        if ( xx(kk) .eq. 1 ) go to 390
  370 continue
  380 c = cwf
      go to 580
  390 r = c
      c = c + w(kk)
      val = val - p(kk)
      xx(kk) = 0
      if ( r .lt. int(minw(kk)) ) go to 400
      ii = kk + 1
      go to 100
  400 nn = kk + 1
      ii = kk
c
c try to substitute the nn-th item for the kk-th.
c
  410 if ( nn .gt. n ) go to 360
      if ( z .ge. val + c*p(nn)/w(nn) ) go to 360
      if ( iubf0 .lt. 0 ) go to 420
      if ( nn .eq. n ) call newb(c,val,minw0,p(n),w(n),fp1,fpn1,
     &                           fw1,iubf0)
  420 diff = w(nn) - w(kk)
      if ( diff ) 530, 430, 440
  430 nn = nn + 1
      go to 410
  440 if ( diff .le. r ) go to 450
      if ( iubf0 .lt. 0 ) go to 430
      a = w(nn) - c
      ib = float(val + p(nn)) - a*fp1/fw1
      if ( ib .gt. iubf0 ) iubf0 = ib
      go to 430
  450 if ( iubf0 .lt. 0 ) go to 480
c
c compute the bound corresponding to the insertion of items
c nn+1,...,n.
c
      npro = val + p(nn)
      ncw = c - w(nn)
      if ( nn .eq. n ) go to 470
      nn1 = nn + 1
      do 460 j=nn1,n
        a = w(j) - ncw
        ib = float(npro + p(j)) - a*fp1/fw1
        if ( ib .gt. iubf0 ) iubf0 = ib
  460 continue
  470 call newb(ncw,npro,minw0,p(n),w(n),fp1,fpn1,fw1,iubf0)
  480 if ( z .ge. val + p(nn) ) go to 430
      z = val + p(nn)
      do 500 j=1,kk
        if ( xx(j) .eq. 0 ) go to 490
        x(j) = iabs(x(j))
        go to 500
  490   x(j) = - iabs(x(j))
  500 continue
      jj = kk + 1
      do 510 j=jj,n
        x(j) = - iabs(x(j))
  510 continue
      x(nn) = iabs(x(nn))
      if ( z .ne. ub ) go to 520
      c = cwf
      go to 580
  520 r = r - diff
      kk = nn
      nn = nn + 1
      go to 410
  530 t = r - diff
      if ( t .ge. int(minw(nn)) ) go to 560
      if ( iubf0 .lt. 0 ) go to 430
c
c compute the bound corresponding to the insertion of items
c nn+1,...,n.
c
      npro = val + p(nn)
      ncw = c - w(nn)
      if ( nn .eq. n ) go to 550
      nn1 = nn + 1
      do 540 j=nn1,n
        if ( z .ge. npro + ncw*p(j)/w(j) ) go to 430
        a = w(j) - ncw
        ib = float(npro + p(j)) - a*fp1/fw1
        if ( ib .gt. iubf0 ) iubf0 = ib
  540 continue
  550 call newb(ncw,npro,minw0,p(n),w(n),fp1,fpn1,fw1,iubf0)
      go to 430
  560 if ( z .ge. val + p(nn) + t*p(nn+1)/w(nn+1) ) go to 360
      c = c - w(nn)
      val = val + p(nn)
      xx(nn) = 1
      ii = nn + 1
      ws(nn) = w(nn)
      ps(nn) = p(nn)
      zs(nn) = ii
      n1 = nn + 1
      do 570 j=n1,lold
        ws(j) = 0
        ps(j) = 0
        zs(j) = j
  570 continue
      lold = nn
      go to 100
  580 if ( iubf0 .lt. 0 ) iubf0 = ub
      if ( iubf0 .lt. z ) iubf0 = z
      return
      end
      subroutine kpmax(ng,pg,wg,cg,zg,xg,mubf,
     &                 jdimc1,jdimc,xxg,min,psign,wsign,zsign)

c*********************************************************************72
c
cc KPMAX solves a 0-1 single knapsack problem using an initial solution.
c
c  Discussion:
c
c    ZG is an initial value of the solution.
c
c    This is a modified version of MT1.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer pg(jdimc1),wg(jdimc1),xg(jdimc),cg,zg
      integer xxg(jdimc),min(jdimc),psign(jdimc),wsign(jdimc),
     &        zsign(jdimc)
      integer cgs,cgf,diff,profit,r,t
c
c initialize.
c
      cgf = cg
      ip = 0
      cgs = cg
      do ll=1,ng
        if ( wg(ll) .gt. cgs ) go to 20
        ip = ip + pg(ll)
        cgs = cgs - wg(ll)
      end do
   20 ll = ll - 1
      if ( cgs .eq. 0 ) go to 50
      pg(ng+1) = 0
      wg(ng+1) = cg + 1
      lim = ip + cgs*pg(ll+2)/wg(ll+2)
      a = wg(ll+1) - cgs
      lim1 = float(ip) - a*float(pg(ll))/float(wg(ll)) +
     &       float(pg(ll+1))
      if ( lim1 .gt. lim ) lim = lim1
      if ( lim .le. zg ) return
      lem = ip + cgs*pg(ll+1)/wg(ll+1)
      if ( lem .le. mubf ) go to 400
      mink = cg + 1
      min(ng) = mink
      do 30 j=2,ng
        kk = ng + 2 - j
        if ( wg(kk) .lt. mink ) mink = wg(kk)
        min(kk-1) = mink
   30 continue
      do 40 j=1,ng
        xxg(j) = 0
   40 continue
      profit = 0
      lold = ng
      ii = 1
      go to 170
   50 if ( zg .ge. ip ) return
      zg = ip
      do j=1,ll
        xg(j) = 1
      end do
      nn = ll + 1
      do j=nn,ng
        xg(j) = 0
      end do
      return
c
c try to insert the ii-th item into the current solution.
c
   80 if ( wg(ii) .le. cg ) go to 90
      ii1 = ii+1
      if ( zg .ge. cg*pg(ii1)/wg(ii1) + profit ) go to 280
      ii = ii1
      go to 80
c
c build a new current solution.
c
   90 ip = psign(ii)
      cgs = cg - wsign(ii)
      in = zsign(ii)
      do 100 ll=in,ng
        if ( wg(ll) .gt. cgs ) go to 160
        ip = ip + pg(ll)
        cgs = cgs - wg(ll)
  100 continue
      ll = ng
  110 if ( zg .ge. ip + profit ) go to 280
      zg = ip + profit
      nn = ii - 1
      do 120 j=1,nn
        xg(j) = xxg(j)
  120 continue
      do 130 j=ii,ll
        xg(j) = 1
  130 continue
      if ( ll .eq. ng ) go to 150
      nn = ll + 1
      do 140 j=nn,ng
        xg(j) = 0
  140 continue
  150 if ( zg .ne. lim ) go to 280
      cg = cgf
      return
  160 ll = ll - 1
      if ( cgs .eq. 0 ) go to 110
      if ( zg .ge. profit + ip + cgs*pg(ll+1)/wg(ll+1) ) go to 280
c
c save the current solution.
c
  170 wsign(ii) = cg - cgs
      psign(ii) = ip
      zsign(ii) = ll + 1
      xxg(ii) = 1
      nn = ll - 1
      if ( nn .lt. ii) go to 190
      do 180 j=ii,nn
        wsign(j+1) = wsign(j) - wg(j)
        psign(j+1) = psign(j) - pg(j)
        zsign(j+1) = ll + 1
        xxg(j+1) = 1
  180 continue
  190 j1 = ll + 1
      do j=j1,lold
        wsign(j) = 0
        psign(j) = 0
        zsign(j) = j
      end do
      lold = ll
      cg = cgs
      profit = profit + ip
      if ( ll - (ng - 2) ) 240, 220, 210
  210 ii = ng
      go to 250
  220 if ( cg .lt. wg(ng) ) go to 230
      cg = cg - wg(ng)
      profit = profit + pg(ng)
      xxg(ng) = 1
  230 ii = ng - 1
      go to 250
  240 ii = ll + 2
      if ( cg .ge. min(ii-1) ) go to 80
c
c save the current optimal solution.
c
  250 if ( zg .ge. profit ) go to 270
      zg = profit
      do j=1,ng
        xg(j) = xxg(j)
      end do
      if ( zg .ne. lim ) go to 270
      cg = cgf
      return
  270 if ( xxg(ng) .eq. 0 ) go to 280
      xxg(ng) = 0
      cg = cg + wg(ng)
      profit = profit - pg(ng)
c
c backtrack.
c
  280 nn = ii - 1
      if ( nn .eq. 0 ) return
      do 290 jj=1,nn
        index = ii - jj
        if ( xxg(index) .eq. 1 ) go to 300
  290 continue
      return
  300 kk = ii - jj
      r = cg
      cg = cg + wg(kk)
      profit = profit - pg(kk)
      xxg(kk) = 0
      if ( r .lt. min(kk) ) go to 310
      ii = kk + 1
      go to 80
  310 nn = kk + 1
      ii = kk
c
c try to substitute the nn-th item for the kk-th.
c
  320 if ( zg .ge. profit + cg*pg(nn)/wg(nn) ) go to 280
      diff = wg(nn) - wg(kk)
      if ( diff ) 380, 330, 340
  330 nn = nn + 1
      go to 320
  340 if ( diff .gt. r ) go to 330
      if ( zg .ge. profit + pg(nn) ) go to 330
      zg = profit + pg(nn)
      do 350 j=1,kk
        xg(j) = xxg(j)
  350 continue
      jj = kk + 1
      do 360 j=jj,ng
        xg(j) = 0
  360 continue
      xg(nn) = 1
      if ( zg .ne. lim ) go to 370
      cg = cgf
      return
  370 r = r - diff
      kk = nn
      nn = nn + 1
      go to 320
  380 t = r - diff
      if ( t .lt. min(nn) ) go to 330
      if ( zg .ge. profit + pg(nn) + t*pg(nn+1)/wg(nn+1) ) go to 280
      cg = cg - wg(nn)
      profit = profit + pg(nn)
      xxg(nn) = 1
      ii = nn + 1
      wsign(nn) = wg(nn)
      psign(nn) = pg(nn)
      zsign(nn) = ii
      n1 = nn + 1
      do 390 j=n1,lold
        wsign(j) = 0
        psign(j) = 0
        zsign(j) = j
  390 continue
      lold = nn
      go to 80
  400 zg = mubf
      do 410 j=1,ng
        xg(j) = 0
  410 continue
      return
      end
      subroutine kpmin(kk,pen,u,d,zp,iy,kubf,
     &                 jdimc,jdimc1,p,w,ind,ix,pw,
     &                 dmyc1,dmyc2,dmyc3,dmyc4,dmyc5)

c*********************************************************************72
c
cc KPMIN solves a 0-1 single knapsack problem in minimization form.
c
c  It uses a subroutine, kpmax, for the corresponding maximization form.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer pen(jdimc),u(jdimc),iy(jdimc),d,zp
      integer p(jdimc1),w(jdimc1),ind(jdimc),ix(jdimc),ds,su,spen
      real    pw(jdimc)
      integer dmyc1(jdimc),dmyc2(jdimc),dmyc3(jdimc),dmyc4(jdimc),
     &        dmyc5(jdimc)

      su = 0
      spen = 0

      do k=1,kk
        su = su + u(k)
        pw(k) = float(pen(k))/float(u(k))
        ix(k) = k
        spen = spen + pen(k)
      end do

      ds = su - d
      i = 0
      iu = 0
      ip = 0

      call sortr(kk,pw,ix,jdimc)

      do jk=1,kk

        k = ix(jk)
        iy(k) = 1

        if ( u(k) .le. ds ) then
          i = i + 1
          p(i) = pen(k)
          w(i) = u(k)
          ip = ip + p(i)
          iu = iu + w(i)
          ind(i) = k
        end if

      end do

      if ( iu .le. ds ) go to 60
      izm = 0

      if ( i .eq. 0 ) then
        zp = spen - izm
        return
      end if

      izm = - 1
      call kpmax(i,p,w,ds,izm,ix,spen-kubf,
     &           jdimc+1,jdimc,dmyc1,dmyc2,dmyc3,dmyc4,dmyc5)
   30 do j=1,i
        k = ind(j)
        iy(k) = 1 - ix(j)
      end do
      zp = spen - izm
      return

   60 do j=1,i
        ix(j) = 1
      end do
      izm = ip
      go to 30

      end
      subroutine ksmall(n,s,k,n6,ss)

c*********************************************************************72
c
cc KSMALL finds the  k-th  smallest of  n  elements in  o(n)  time.
c
c entrance to ksmall is achieved by using the statement
c        call ksmall(n,s,k,(n+5)/6,ss)
c
c the values of the first three parameters must be defined
c by the user prior to calling ksmall. ksmall needs one
c array  ( s )  of length  n  and one array ( ss )  of
c length  (n+5)/6  .these arrays must be dimensioned by the
c user in the calling program.
c
c ksmall calls eight subroutines: bld, bldf, blds1, detns1,
c detns2, forwd, mpsort and sort7.
c these subroutines are completely local, i.e. the informa-
c tion they need is passed through the parameter list.
c the whole program is completely self contained and commu-
c nication with it is achieved solely through the parameter
c list of ksmall. no machine dependent costants are used.
c the program is written in american national standard
c fortran and is accepted by the pfort verifier.
c the program has been tested on a  cdc cyber 76 , on a  cdc
c cyber 730  and on a  digital vax 11/780 .
c
c meaning of the input parameters:
c n = number of elements.
c s = array containing the elements.
c k = integer value indicating that the  k-th  smallest
c     element of  s  must be found ( 1 .le. k .le. n ) .
c
c n ,  k  and  n6  are integer,  s  and  ss  are real.
c on return, the elements of  s  are rearranged so that
c s(k)  contains the  k-th  smallest element of  s , while
c s(i) .le. s(k)  if  i .lt. k ,  s(i) .ge. s(k)  if
c i .gt. k .
c the current dimensions of work arrays  l ,  r  and  t
c allow use of the code for practically any value of  n
c ( n .lt. 98*7**5 ).
c
c in the following, the comment sections refer to procedure
c ksmall described in  "a hybrid algorithm for finding
c the  k-th  smallest of  n  elements in  o(n)  time" , by
c m. fischetti and s. martello, annals of operational
c research 13, 1988.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension s(n),ss(n6)
      integer l(6),r(6),t(6)
      l(1) = 1
      r(1) = n
      t(1) = k
      lev = 1
c
c statements  1 - 10 .
c
   10 if ( lev .gt. 1 )  go to 20
      call forwd(n,s,n6,ss,lev,l,r,t,1,v,jflag)
      go to 30
   20 call forwd(n6,ss,n6,ss,lev,l,r,t,r(lev)+1,v,jflag)
   30 if ( jflag .eq. 0 ) go to 20
   40 lev = lev - 1
      if ( lev .eq. 0 ) return
      il = l(lev)
      ir = r(lev)
      it = t(lev)
      nn = ir - il + 1
      nn7 = nn/7
      ii = il + 7*(nn7 - 1)
      nxt = 1
      if ( lev .gt. 1 ) nxt = ir + 1
c
c statements  11 - 13 .
c
c compute  ns1 = cardinality of set  a1 .
c
      if ( lev .gt. 1 )  go to 50
      call detns1(n,s,n6,ss,il,ir,ii,nxt,v,ns1,nlr)
      go to 60
   50 call detns1(n6,ss,n6,ss,il,ir,ii,nxt,v,ns1,nlr)
   60 if ( ns1 .lt. it ) go to 90
c
c explicitly determine set  a1 .
c
      if ( lev .gt. 1 ) go to 70
      call bldf(n,s,n6,ss,il,ir,nlr)
      go to 80
   70 call blds1(n6,ss,il,ii,nxt,nlr)
   80 r(lev) = il + ns1 - 1
      go to 10
   90 if ( ns1 .lt. 11*nn/70 ) go to 110
c
c explicitly determine set  a - a1 .
c
      t(lev) = it - ns1
      if ( lev .gt. 1 ) go to 100
      call bldf(n,s,n6,ss,il,ir,nlr)
      l(lev) = il + ns1
      go to 10
  100 call bld(n6,ss,il,ir,ii,nxt,nlr)
      r(lev) = il + ( nn - ns1 ) - 1
      go to 10
c
c statements  14 - 16 .
c
c compute  ns2 = cardinality of set  a2 .
c
  110 if ( lev .gt. 1 ) go to 120
      call detns2(n,s,n6,ss,il,ir,ii,nxt,v,ns2,nlr)
      go to 130
  120 call detns2(n6,ss,n6,ss,il,ir,ii,nxt,v,ns2,nlr)
  130 ns12 = ns1 + ns2
      if ( ns12 .lt. it ) go to 160
      if ( lev .gt. 1 ) go to 40
      call bldf(n,s,n6,ss,il,ir,nlr)
      do 140 i=1,ns12
        if ( s(i) .eq. v ) go to 150
  140 continue
  150 s(i) = s(k)
      s(k) = v
      return
c
c explicitly determine set  a - a1 - a2 .
c
  160 t(lev) = it - ns12
      if ( lev .gt. 1 ) go to 170
      call bldf(n,s,n6,ss,il,ir,nlr)
      l(lev) = il + ns12
      go to 10
  170 call bld(n6,ss,il,ir,ii,nxt,nlr)
      r(lev) = il + ( nn - ns12 ) - 1
      go to 10
      end
      subroutine l2(n,w,c,lb,jdim)

c*********************************************************************72
c
cc L2 computes the lower bound.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim),c
      if ( w(1) .gt. c/2 ) go to 40
c
c case 1: all items are .le. c/2 .
c
      lb = 0
      irat = c/w(1)
      ii = 2
   10 do 20 i=ii,n
        ir = c/w(i)
        if ( ir .gt. irat ) go to 30
   20 continue
      nlb = (n - 1)/irat + 1
      if ( lb .lt. nlb ) lb = nlb
      return
   30 nlb = (i - 2)/irat + 1
      if ( lb .lt. nlb ) lb = nlb
      if ( (n - 1)/ir + 1 .le. lb ) return
      ii = i
      irat = ir
      go to 10
c
c case 2: there exist items  .gt. c/2 .
c
   40 call search(n,w,float(c)/2.,n12,jdim)
c
c n12 = n1 + n2 .
c
      lb = n12
      if ( n12 .eq. n ) return
      n12p1 = n12 + 1
      nmn12 = n - n12
      jbw = c - w(n12)
c
c i2 = next item to be considered for possible inclusion in  n2 .
c i3 = next item to be considered for possible inclusion in  n3 .
c
      i2 = n12
      i3 = n12p1
   50 jww = w(i3)
   60 i3 = i3 + 1
      if ( i3 .gt. n ) go to 70
      if ( w(i3) .ge. jww ) go to 60
   70 jwst = w(i3-1)
      jbwst = c - jwst
      n3 = i3 - n12p1
   80 if ( w(i2) .gt. jbwst ) go to 100
      if ( i2 .eq. 1 ) go to 90
      i2 = i2 - 1
      go to 80
   90 n2 = n12
      go to 110
  100 n2 = n12 - i2
  110 nn = n2*(jbw/jwst)
      jadd = 0
      if ( n3 .gt. nn ) jadd = (n3 - nn - 1)/(c/jwst) + 1
      nlb = n12 + jadd
      if ( lb .lt. nlb ) lb = nlb
      nadd = (nmn12 - nn - 1)/(c/jwst) + 1
      if ( n12 + nadd .le. lb ) return
      if ( i3 .gt. n ) return
      go to 50
      end
      subroutine l3(n,w,c,zz,mred,k,x,nfreer,lb,ub,xheu,isum,nub,
     &              res,rel,jdim)

c*********************************************************************72
c
cc L3 reduces the current problem, and compute lower bound  l3  and a new upper bound  nub .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c on output:
c nfreer    = number of unassigned variables;
c mred      = number of bins used in the reduction without relaxation;
c x(i)      = bin in which item i has been inserted by the reduction
c             procedure without relaxation,
c           = 0 if item i has not been assigned
c             (during execution,  x(i)  is used as a pointer to the
c             next free item following  i  or as the bin in which
c             item  i  has been inserted with relaxation);
c xheu(i)   = bin in which item  i  has been inserted in the heuristic
c             solution corresponding to nub
c             (during execution,  xheu(i)  contains the level of
c             relaxation at which item  i  has been assigned;
c             if  xheu(i) .gt. n , item  i  has been relaxed);
c res(l)    = residual capacity of bin  l (l=1,...,m);
c nrel      = number of relaxed items;
c rel(irel) = irel-th relaxed item (irel=1,...,nrel);
c krel      = level of relaxation;
c mrel      = maximum level of relaxations for which the reduction is
c             valid;
c kinf      = n + 1  if no pair of bins has been matched,
c           = level at which the first pair of bins has been matched,
c             otherwise.
c
c the items from  1  to  zz  represent bins partially filled,
c those from  zz + 1  to  n  are real items.
c the subroutine terminates and returns  nfree = - 1  if it
c attempts to match pairs of bins.
c
      integer w(jdim),c,k(jdim),x(jdim),xheu(jdim),zz,ub
      integer res(jdim),rel(jdim),fs,tp,ww,qp,www
c
c initialization.
c
      lb = 0
      isumr = isum
      mp = 0
      kinf = n + 1

      do i=1,zz
        res(i) = 0
      end do
   20 do 30 i=1,n
        x(i) = i + 1
        k(i) = i - 1
        xheu(i) = 0
   30 continue
      krel = 0
      mrel = - 1
      mred = zz
      nfreer = n
      nrel = 0
      x(n) = 0
      fs = 1
      m = zz
      nfree = n
      i = 0
      ifp = n
      if ( n .le. 2 ) go to 470
   40 i = fs
      jj = ifp
c
c try to assign  w(i) .
c
   50   jwb = c - w(i)
        ii = x(i)
c
c first test.
c
        if ( w(ifp) .le. jwb ) go to 90
c
c insert only item  i .
c
        mm = i
        if ( i .le. zz ) go to 60
        m = m + 1
        mm = m
   60   is = x(i)
        ip = k(i)
        k(is) = ip
        if ( ip .gt. 0 ) go to 70
        fs = is
        go to 80
   70   x(ip) = is
   80   x(i) = mm
        xheu(i) = krel
        mp = mp + 1
        res(mm) = c - w(i)
        isumr = isumr - w(i)
        nfree = nfree - 1
        go to 450
c
c find the largest free  w(jj)  which fits into a bin together
c with w(i)  ( jj .gt. i ) .
c
   90   j = jj
  100   if ( j .eq. i ) go to 110
        if ( w(j) .gt. jwb ) go to 120
        j = k(j)
        go to 100
  110   if ( i .eq. fs ) go to 120
        ij = k(i)
        if ( w(ij) .gt. jwb ) go to 120
        jj = x(j)
        go to 460
c
c second test.
c
  120   jj = x(j)
        if ( w(jj) .eq. jwb ) go to 360
c
c fourth test.
c
        isp = k(ifp)
        if ( w(ifp) + w(isp) .gt. jwb ) go to 360
        if ( i .eq. isp ) go to 360
c
c fifth test.
c
        llp = ifp
        tp = k(isp)
        if ( tp .eq. i ) go to 130
        if ( w(ifp) + w(tp) .le. jwb ) go to 240
c
c only pair  (isp,ifp)  can be inserted with item  i .
c
        if ( w(jj) .ge. w(ifp) + w(isp) ) go to 360
        if ( w(jj) .ne. w(isp) ) go to 460
c
c insert items  i ,  jj  and  llp .
c
  130   if ( jj .gt. zz ) go to 140
        if ( krel .lt. kinf ) kinf = krel
  140   mm = i
        if ( i .le. zz ) go to 150
        m = m + 1
        mm = m
  150   is = x(i)
        ip = k(i)
        k(is) = ip
        if ( ip .gt. 0 ) go to 160
        fs = is
        go to 170
  160   x(ip) = is
  170   x(i) = mm
        xheu(i) = krel
        mp = mp + 1
        iw = w(i) + w(jj) + w(llp)
        res(mm) = c - iw
        isumr = isumr - iw
        if ( jj .eq. ii ) ii = x(ii)
        if ( ii .eq. 0 ) ii = ifp
        js = x(jj)
        jp = k(jj)
        if ( jp .gt. 0 ) go to 180
        fs = js
        go to 190
  180   x(jp) = js
  190   k(js) = jp
        x(jj) = mm
        xheu(jj) = krel
        jj = js
        if ( llp .eq. ii ) ii = x(ii)
        if ( ii .eq. 0 ) ii = ifp
        ls = x(llp)
        lp = k(llp)
        if ( lp .gt. 0 ) go to 200
        fs = ls
        go to 210
  200   x(lp) = ls
  210   if ( ls .gt. 0 ) go to 220
        ifp = lp
        go to 230
  220   k(ls) = lp
  230   x(llp) = mm
        xheu(llp) = krel
        if ( jj .eq. llp ) jj = ls
        if ( ls .eq. 0 ) jj = ifp
        nfree = nfree - 3
        go to 450
c
c sixth test.
c
  240   www = 0
        if ( w(ifp) + w(isp) + w(tp) .gt. jwb ) go to 280
        qp = k(tp)
        if ( qp .eq. i ) go to 250
        if ( w(ifp) + w(isp) + w(qp) .le. jwb ) go to 550
c
c only triplet  (tp,isp,ifp)  can be inserted with item  i .
c
        if ( w(jj) .eq. w(tp) ) go to 250
        www = w(ifp) + w(isp) + w(tp)
        go to 280
c
c insert items  i ,  tp ,  isp ,  ifp .
c
  250   if ( tp .gt. zz ) go to 260
        if ( krel .lt. kinf ) kinf = krel
  260   mm = i
        if ( i .le. zz ) go to 270
        m = m + 1
        mm = m
  270   call insert(i,mm,fs,x,ifp,k,jdim)
        call insert(tp,mm,fs,x,ifp,k,jdim)
        call insert(isp,mm,fs,x,ifp,k,jdim)
        lifp = ifp
        call insert(lifp,mm,fs,x,ifp,k,jdim)
        xheu(i) = krel
        xheu(tp) = krel
        xheu(isp) = krel
        xheu(lifp) = krel
        mp = mp + 1
        iw = w(i) + w(tp) + w(isp) + w(lifp)
        res(mm) = c - iw
        isumr = isumr - iw
        if ( jj .eq. ii) ii = ifp
        jj = ifp
        nfree = nfree - 4
        go to 450
c
c find the best pair  (kkp,llp)  of total weight  ww  which can be
c inserted into a bin together with item  i .
c
  280   kk = k(jj)
        ll = ifp
        ww = 0
  290   ll1 = k(ll)
        if ( kk. ge. ll1 ) go to 340
        j = kk
        jwl = jwb - w(ll)
  300   j = x(j)
        if ( j .ge. ll ) go to 340
        if ( w(j) .gt. jwl ) go to 300
        kk = j
        j = ll
        jwk = jwb - w(kk)
  310   j = k(j)
        if ( j .le. kk ) go to 320
        if ( w(j) .le. jwk ) go to 310
  320   ll = x(j)
        if ( w(kk) + w(ll) .le. ww ) go to 330
        ww = w(kk) + w(ll)
        kkp = kk
        llp = ll
        if ( w(jj) .ge. ww ) go to 330
        if ( w(jj) .ne. w(kkp) ) go to 460
  330   ll = k(ll)
        go to 290
  340   if ( (w(jj) .ge. ww) .and. (w(jj) .ge. www) ) go to 360
        if ( w(jj) .ne. w(kkp) ) go to 460
c
c item  jj  is in the best pair (jj=kkp).
c check whether pair  (kkp,llp)  dominates all the feasible pairs.
c
        jll = k(llp)
        jjll = k(jll)
        if ( jjll .le. jj ) go to 350
        if ( w(jll) + w(jjll) .le. jwb ) go to 460
  350   if ( www .eq. 0 ) go to 130
c
c check whether  pair  (kkp,llp)  dominates triplet  (tp,isp,ifp) .
c
        if ( w(llp) .lt. w(isp) ) go to 460
        if ( w(kkp) + w(llp) .ge. www ) go to 130
        go to 460
c
c insert items  i  and  jj .
c
  360   if ( jj .gt. zz ) go to 370
        if ( krel .lt. kinf ) kinf = krel
  370   mm = i
        if ( i .le. zz ) go to 380
        m = m + 1
        mm = m
  380   is = x(i)
        ip = k(i)
        k(is) = ip
        if ( ip .gt. 0 ) go to 390
        fs = is
        go to 400
  390   x(ip) = is
  400   x(i) = mm
        xheu(i) = krel
        mp = mp + 1
        iw = w(i) + w(jj)
        res(mm) = c - iw
        isumr = isumr - iw
        if ( jj .eq. ii ) ii = x(ii)
        if ( ii .eq. 0 ) ii = ifp
        js = x(jj)
        jp = k(jj)
        if ( jp .gt. 0 ) go to 410
        fs = js
        go to 420
  410   x(jp) = js
  420   if ( js .gt. 0 ) go to 430
        ifp = jp
        go to 440
  430   k(js) = jp
  440   x(jj) = mm
        xheu(jj) = krel
        jj = js
        if ( js .eq. 0 ) jj = ifp
        nfree = nfree - 2
c
c stopping test.
c
  450   if ( nfree .le. 2 ) go to 470
  460   i = ii
      if ( i .lt. ifp ) go to 50
      go to 670
c
c optimal solution.
c
  470 if ( nfree .eq. 0 ) go to 670
      i1 = 0
      i = fs
  480   ii = x(i)
        if ( i1 .gt. 0 ) go to 500
        i1 = i
        mm = i
        if ( i .le. zz ) go to 490
        m = m + 1
        mm = m
  490   x(i1) = mm
        xheu(i1) = krel
        mp = mp + 1
        res(mm) = c - w(i1)
        isumr = isumr - w(i1)
        if ( nfree .eq. 1 ) go to 540
        go to 530
  500   if ( w(i) .gt. c - w(i1) ) go to 510
        if ( i .le. zz ) go to 510
        x(i) = mm
        xheu(i) = krel
        res(mm) = res(mm) - w(i)
        isumr = isumr - w(i)
        go to 540
  510   mm = i
        if ( i .le. zz ) go to 520
        m = m + 1
        mm = m
  520   x(i) = mm
        xheu(i) = krel
        mp = mp + 1
        res(mm) = c - w(i)
        isumr = isumr - w(i)
        go to 540
  530   i = ii
      go to 480
  540 nfree = 0
      fs = 0
      go to 670
c
c only second test for the remaining items.
c
  550 if ( krel .gt. 0 ) go to 670
      j = k(ifp)
      jb2 = (c + 1)/2
  560 i = ii
      if ( i .eq. ifp ) go to 670
      if ( w(i) .lt. jb2 ) go to 670
      ii = x(i)
      jwb = c - w(i)
  570 if ( j .eq. i ) go to 670
      if ( w(j) .gt. jwb ) go to 560
      if ( w(j) .eq. jwb ) go to 580
      j = k(j)
      go to 570
c
c insert items  i  and  j .
c
  580 if ( j .gt. zz ) go to 590
      if ( krel .lt. kinf ) kinf = krel
  590 mm = i
      if ( i .le. zz ) go to 600
      m = m + 1
      mm = m
  600 is = x(i)
      ip = k(i)
      k(is) = ip
      if ( ip .gt. 0 ) go to 610
      fs = is
      go to 620
  610 x(ip) = is
  620 x(i) = mm
      xheu(i) = krel
      mp = mp + 1
      iw = w(i) + w(j)
      res(mm) = c - iw
      isumr = isumr - iw
      if ( j .eq. ii ) ii = x(ii)
      if ( ii .eq. 0 ) ii = ifp
      js = x(j)
      jp = k(j)
      if ( jp .gt. 0 ) go to 630
      fs = js
      go to 640
  630 x(jp) = js
  640 if ( js .gt. 0 ) go to 650
      ifp = jp
      go to 660
  650 k(js) = jp
  660 x(j) = mm
      xheu(j) = krel
      j = jp
      nfree = nfree - 2
      if ( nfree .le. 2 ) go to 470
      go to 560
  670 if ( nfree .eq. 0 ) go to 680
      if ( w(fs) + w(ifp) .gt. c ) go to 40
c
c check whether all the previously relaxed items can be inserted
c into the bins currently used by the reduced items.
c
  680 if ( kinf .le. n ) go to 710
      if ( nrel .eq. 0 ) go to 700
      ix = rel(nrel)
c
c check whether the last relaxed item  ix  can be inserted into
c a feasible bin.
c
      iw = w(ix)
      min = c + 1
      do 690 l=1,m
        if ( iw .gt. res(l) ) go to 690
        if ( res(l) .ge. min ) go to 690
        min = res(l)
        lmin = l
  690 continue
c
c if  min .gt. c  then item  ix  cannot be inserted in any current bin.
c
      if ( min .gt. c ) go to 710
c
c insert item  ix  in bin  lmin  and consider the next relaxed item.
c
      x(ix) = lmin
      xheu(ix) = krel
      res(lmin) = res(lmin) - iw
      nrel = nrel - 1
      go to 680
c
c all the relaxed items have been inserted in the current bins.
c
  700 mrel = krel
      mred = m
      nfreer = nfree
c
c compute the current lower bound  lbc .
c
  710 lbc = mp + ( isumr + c - 1 )/c
      if ( krel .eq. 0 .and. kinf .le. n ) go to 780
      if ( lb .lt. lbc ) lb = lbc
      if ( lb .ge. ub ) return
      if ( nfree .eq. 0 ) go to 720
c
c relax the last item  ifp .
c
      krel = krel + 1
      xheu(ifp) = n + 1
      isumr = isumr - w(ifp)
      nrel = nrel + 1
      rel(nrel) = ifp
      ifp = k(ifp)
      x(ifp) = 0
      nfree = nfree - 1
      go to 40
c
c define  lb ,  x(i) ,  xheu(i)  and  nub .
c
  720 if ( lb .lt. mp ) lb = mp
      nub = m
      do 770 i=1,n
        if ( xheu(i) .gt. mrel ) go to 730
        xheu(i) = x(i)
        go to 770
c
c item  i  has not been reduced in a valid way.
c
  730   if ( xheu(i) .gt. n ) go to 740
        xheu(i) = x(i)
        x(i) = 0
        go to 770
c
c try to insert the relaxed item  i  into an existing bin
c or initialize a new bin.
c
  740   iw = w(i)
        min = c + 1
        do 750 l=1,nub
          if ( iw .gt. res(l) ) go to 750
          if ( min .le. res(l) ) go to 750
          min = res(l)
          lmin = l
  750   continue
        if ( min .gt. c ) go to 760
        xheu(i) = lmin
        x(i) = 0
        res(lmin) = res(lmin) - w(i)
        go to 770
  760   nub = nub + 1
        xheu(i) = nub
        x(i) = 0
        res(nub) = c - w(i)
  770 continue
      return
  780 nfreer = - 1
      return
      end
      subroutine lcl2(n,w,c,isum,r,fixit,zz,z,k,na,wa,wb,llb,jdim)

c*********************************************************************72
c
cc LCL2 computes a local lower bound and execute a preprocessing for reduction.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim),r(jdim),wa(jdim),wb(jdim),fixit(jdim),c,zz,z

      i = n
   10 if ( fixit(i) .eq. 0 ) go to 20
      i = i - 1
      go to 10
   20 jwn = w(i)
      iss = isum
      kcb = 0

      do 30 j=1,zz
        wb(j) = j
        if ( r(j) .ge. jwn ) go to 30
        iss = iss - (c - r(j))
        kcb = kcb + 1
   30 continue

      llb = kcb + (iss - 1)/c + 1
      if ( llb .ge. z ) return

      call sorti2(zz,r,wb,jdim)

      do i=1,zz
        iwbi = wb(i)
        wa(i) = c - r(iwbi)
      end do

      k1 = k + 1
      na = zz
      do 50 i=k1,n
        if ( fixit(i) .ne. 0 ) go to 50
        na = na + 1
        wa(na) = w(i)
        wb(na) = i
   50 continue
      call l2(na,wa,c,lba,jdim)
      if ( lba .gt. llb ) llb = lba
      return
      end
      subroutine maxt(n,w,i1,i2,i3,jdn)

c*********************************************************************72
c
cc MAXT determines the three items of maximum weight.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdn)

      max1 = -1
      max2 = -1
      max3 = -1
      i1 = 0
      i2 = 0

      do 30 i=1,n
        if ( w(i) .le. max3 ) go to 30
        if ( w(i) .gt. max1 ) go to 20
        if ( w(i) .gt. max2 ) go to 10
        max3 = w(i)
        i3 = i
        go to 30
   10   max3 = max2
        max2 = w(i)
        i3 = i2
        i2 = i
        go to 30
   20   max3 = max2
        max2 = max1
        max1 = w(i)
        i3 = i2
        i2 = i1
        i1 = i
   30 continue

      return
      end
      subroutine mgr1(n,p,w,m,c,z,x,cr,inf,jdn,jdm)

c*********************************************************************72
c
cc MGR1 finds an initial solution (quick algorithm).
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdn),w(jdn),c(jdm),x(jdn),cr(jdm),z

      do i=1,m
        cr(i) = c(i)
      end do

      mp1 = m + 1
      p(n+1) = 0
      w(n+1) = inf
      j = 1
      i = 1
      z = 0
   20 if ( w(j) .gt. cr(i) ) go to 30
      x(j) = i
      cr(i) = cr(i) - w(j)
      z = z + p(j)
      j = j + 1
      go to 20
   30 js = j
      x(j) = mp1
      j = j + 1
      do 40 jj=j,n
        x(jj) = mp1
        if ( w(jj) .gt. cr(i) ) go to 40
        x(jj) = i
        cr(i) = cr(i) - w(jj)
        z = z + p(jj)
   40 continue
   50 if ( i .lt. m ) go to 60
      go to 110
   60 i = i + 1
      do 70 j=js,n
        if ( x(j) .le. m ) go to 70
        if ( w(j) .gt. cr(i) ) go to 80
        x(j) = i
        cr(i) = cr(i) - w(j)
        z = z + p(j)
   70 continue
      go to 110
   80 js = j
      j = j + 1
   90 if ( cr(i)*p(j)/w(j) .eq. 0 ) go to 50
      if ( x(j) .le. m ) go to 100
      if ( w(j) .gt. cr(i) ) go to 100
      x(j) = i
      cr(i) = cr(i) - w(j)
      z = z + p(j)
  100 j = j + 1
      go to 90
  110 continue
      return
      end
      subroutine mgr2(n,p,w,m,c,z,x,cr,inf,jdn,jdm)

c*********************************************************************72
c
cc MGR2 finds an initial solution (accurate algorithm).
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdn),w(jdn),c(jdm),x(jdn),cr(jdm),z
      integer s,d,zcap,vzcap,czcap,vcap,zstar,vzstar,czstar,q
      integer a(5,200),v(5),zz(5),cz(5),vz(5),oz(5),b(5),ifb(5),
     &        minw(201)
      z = 0
      jstar = 1
      p(n+1) = 0
      w(n+1) = inf
      mp1 = m + 1
      mink = inf
      minw(n+1) = mink

      do j=1,n
        x(j) = mp1
        kk = n + 1 - j
        if ( w(kk) .lt. mink ) mink = w(kk)
        minw(kk) = mink
        do i=1,m
          a(i,j) = 0
        end do
      end do

      x(n+1) = mp1

      do i=1,m
        zz(i) = 1
        cz(i) = c(i)
        vz(i) = 0
        oz(i) = 0
        b(i) = i
      end do

      ibound = 0
      kb = 0
      mb = m

   40 if ( kb .eq. mb ) go to 170
      kb = kb + 1
      i = b(kb)
      if ( ibound .eq. 0) go to 50
      zcap = zz(i)
      vzcap = vz(i)
      czcap = cz(i)
      vcap = v(i)
      if ( s .ge. zz(i) ) go to 50
      vz(i) = vz(i) - p(s)
      cz(i) = cz(i) + w(s)
   50 j = zz(i)
      cr(i) = cz(i)
      v(i) = vz(i)
   60 if ( cr(i) .lt. minw(j) ) go to 70
      if ( cr(i)*p(j)/w(j) .ge. 1 ) go to 80
   70 zz(i) = j
      cz(i) = cr(i)
      vz(i) = v(i)
      go to 140
   80 if ( w(j) .gt. cr(i) ) go to 90
      cr(i) = cr(i) - w(j)
      v(i) = v(i) + p(j)
      a(i,j) = 1
      ioz = j
      j = j + 1
      go to 60
   90 if ( j .ne. jstar ) go to 100
      a(i,j) = 0
      j = j + 1
      go to 60
  100 zz(i) = j
      cz(i) = cr(i)
      vz(i) = v(i)
  110 if ( cr(i) .lt. minw(j) ) go to 140
      if ( cr(i)*p(j)/w(j) .lt. 1 ) go to 140
      if ( w(j) .gt. cr(i) ) go to 120
      cr(i) = cr(i) - w(j)
      v(i) = v(i) + p(j)
      a(i,j) = 1
      ioz = j
      go to 130
  120 a(i,j) = 0
  130 j = j + 1
      go to 110
  140 jo = oz(i)
      if ( jo .lt. j ) go to 160
      do 150 q=j,jo
        a(i,q) = 0
  150 continue
  160 oz(i) = ioz
      if ( ibound .eq. 0 ) go to 40
      if ( vcap - v(i) .le. d ) go to 40
      d = vcap - v(i)
      istar = i
      zstar = zcap
      vzstar = vzcap
      czstar = czcap
      go to 40
  170 if ( ibound .eq. 1 ) go to 180
      j = jstar
      go to 210
  180 z = z + p(s)
      x(s) = istar
  190 if ( x(jstar) .eq. mp1 ) go to 200
      jstar = jstar + 1
      go to 190
  200 mb = 0
      kb = 0
      ibound = 0
      i = istar
      zz(i) = zstar
      vz(i) = vzstar
      cz(i) = czstar
      go to 50
  210 if ( j .gt. n ) return
      mb = 0
      do 220 i=1,m
        ifb(i) = 0
        if ( a(i,j) .eq. 0 ) go to 220
        mb = mb + 1
        b(mb) = i
        ifb(i) = 1
  220 continue
      kb = 0
      if ( mb .le. 1 ) go to 240
      ibound = 1
      s = j
      d = - inf
      jstar = j + 1
      do 230 i=1,m
        if ( zz(i) .ge. jstar ) go to 230
        zz(i) = jstar
        if ( ifb(i) .eq. 0 ) go to 230
        vz(i) = vz(i) + p(s)
        cz(i) = cz(i) - w(s)
  230 continue
      go to 40
  240 if ( mb .eq. 0 ) go to 250
      i = b(1)
      z = z + p(j)
      x(j) = i
      if ( j .lt. zz(i) ) go to 250
      zz(i) = j + 1
      cz(i) = cz(i) - w(j)
      vz(i) = vz(i) + p(j)
  250 j = j + 1
      go to 210
      end
      subroutine mpsort(na,a,i1,i2,it,v)

c*********************************************************************72
c
cc MPSORT rearranges a(i1:i2) so a(it+i1-1) contains the  it-th  smallest element.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension a(na)
      i = i1
      j = i2
      ita = it + i1 - 1
   10 if ( i .lt. j ) go to 20
      v = a(ita)
      return
   20 irr = i
      ij = (i + j)/2
      ap = a(ij)
      if ( a(i) .le. ap ) go to 30
      a(ij) = a(i)
      a(i) = ap
      ap = a(ij)
   30 ill = j
      if ( a(j) .ge. ap ) go to 50
      a(ij) = a(j)
      a(j) = ap
      ap = a(ij)
      if ( a(i) .le. ap ) go to 50
      a(ij) = a(i)
      a(i) = ap
      ap = a(ij)
      go to 50
   40 a(ill) = a(irr)
      a(irr) = aux
   50 ill = ill - 1
      if ( a(ill) .gt. ap ) go to 50
      aux = a(ill)
   60 irr = irr + 1
      if ( a(irr) .lt. ap ) go to 60
      if ( irr .le. ill ) go to 40
      if ( ita .lt. irr ) go to 70
      i = irr
      go to 80
   70 j = ill
   80 if ( j - i .gt. 10 ) go to 20
      if ( i .eq. i1 ) go to 10
      i = i - 1
   90 i = i + 1
      if ( i .ne. j ) go to 100
      v = a(ita)
      return
  100 ap = a(i+1)
      if ( a(i) .le. ap ) go to 90
      irr = i
  110 a(irr+1) = a(irr)
      irr = irr - 1
      if ( ap .lt. a(irr) ) go to 110
      a(irr+1) = ap
      go to 90
      end
      subroutine mt1(n,p,w,c,z,x,jdim,jck,xx,min,psign,wsign,zsign)

c*********************************************************************72
c
cc MT1 solves the 0-1 single knapsack problem.
c
c maximize  z = p(1)*x(1) + ... + p(n)*x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) .le. c ,
c               x(j) = 0 or 1  for j=1,...,n.
c
c the program implements the branch-and-bound algorithm described in
c section  2.5.2 .
c the program derives from an earlier code presented in
c  s. martello, p. toth, "algorithm for the solution of the 0-1 single
c  knapsack problem", computing, 1978.
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdim - 1 ;
c   2) p(j), w(j), c  positive integers;
c   3) max (w(j)) .le. c ;
c   4) w(1) + ... + w(n) .gt. c ;
c   5) p(j)/w(j) .ge. p(j+1)/w(j+1) for j=1,...,n-1.
c
c mt1 calls  1  procedure: chmt1.
c
c the program is completely self-contained and communication to it is
c achieved solely through the parameter list of mt1.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mt1 needs  8  arrays ( p ,  w ,  x ,  xx ,  min ,  psign ,  wsign
c                        and  zsign ) of length at least  n + 1 .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c n    = number of items;
c p(j) = profit of item  j  (j=1,...,n);
c w(j) = weight of item  j  (j=1,...,n);
c c    = capacity of the knapsack;
c jdim = dimension of the 8 arrays;
c jck  = 1 if check on the input data is desired,
c      = 0 otherwise.
c
c meaning of the output parameters:
c z    = value of the optimal solution if  z .gt. 0 ,
c      = error in the input data (when jck=1) if z .lt. 0 : condi-
c        tion  - z  is violated;
c x(j) = 1 if item  j  is in the optimal solution,
c      = 0 otherwise.
c
c arrays xx, min, psign, wsign and zsign are dummy.
c
c all the parameters are integer. on return of mt1 all the input
c parameters are unchanged.
c
cf2py intent(in) n, p, w, c, jdim, jck
cf2py intent(hide) xx, min, psign, wsign, zsign
cf2py intent(out) z, x
Cf2py depend(jdim) p, w, x, xx, min, psign, wsign, zsign
      integer p(jdim),w(jdim),x(jdim),c,z
      integer xx(jdim),min(jdim),psign(jdim),wsign(jdim),zsign(jdim)
      integer ch,chs,diff,profit,r,t
      z = 0
      if ( jck .eq. 1 ) call chmt1(n,p,w,c,z,jdim)
      if ( z .lt. 0 ) return
c
c initialize.
c
      ch = c
      ip = 0
      chs = ch
      do ll=1,n
        if ( w(ll) .gt. chs ) go to 20
        ip = ip + p(ll)
        chs = chs - w(ll)
      end do

   20 ll = ll - 1
      if ( chs .eq. 0 ) go to 50
      p(n+1) = 0
      w(n+1) = ch + 1
      lim = ip + chs*p(ll+2)/w(ll+2)
      a = w(ll+1) - chs
      b = ip + p(ll+1)
      lim1 = b - a*float(p(ll))/float(w(ll))
      if ( lim1 .gt. lim ) lim = lim1
      mink = ch + 1
      min(n) = mink
      do j=2,n
        kk = n + 2 - j
        if ( w(kk) .lt. mink ) mink = w(kk)
        min(kk-1) = mink
      end do

      do j=1,n
        xx(j) = 0
      end do

      z = 0
      profit = 0
      lold = n
      ii = 1
      go to 170
   50 z = ip
      do j=1,ll
        x(j) = 1
      end do
      nn = ll + 1
      do j=nn,n
        x(j) = 0
      end do
      return
c
c try to insert the ii-th item into the current solution.
c
   80 if ( w(ii) .le. ch ) go to 90
      ii1 = ii + 1
      if ( z .ge. ch*p(ii1)/w(ii1) + profit ) go to 280
      ii = ii1
      go to 80
c
c build a new current solution.
c
   90 ip = psign(ii)
      chs = ch - wsign(ii)
      in = zsign(ii)
      do ll=in,n
        if ( w(ll) .gt. chs ) go to 160
        ip = ip + p(ll)
        chs = chs - w(ll)
      end do

      ll = n
  110 if ( z .ge. ip + profit ) go to 280
      z = ip + profit
      nn = ii - 1
      do 120 j=1,nn
        x(j) = xx(j)
  120 continue
      do 130 j=ii,ll
        x(j) = 1
  130 continue
      if ( ll .eq. n ) go to 150
      nn = ll + 1
      do 140 j=nn,n
        x(j) = 0
  140 continue
  150 if ( z .ne. lim ) go to 280
      return
  160 iu = chs*p(ll)/w(ll)
      ll = ll - 1
      if ( iu .eq. 0 ) go to 110
      if ( z .ge. profit + ip + iu ) go to 280
c
c save the current solution.
c
  170 wsign(ii) = ch - chs
      psign(ii) = ip
      zsign(ii) = ll + 1
      xx(ii) = 1
      nn = ll - 1
      if ( nn .lt. ii) go to 190
      do 180 j=ii,nn
        wsign(j+1) = wsign(j) - w(j)
        psign(j+1) = psign(j) - p(j)
        zsign(j+1) = ll + 1
        xx(j+1) = 1
  180 continue
  190 j1 = ll + 1
      do j=j1,lold
        wsign(j) = 0
        psign(j) = 0
        zsign(j) = j
      end do
      lold = ll
      ch = chs
      profit = profit + ip
      if ( ll - (n - 2) ) 240, 220, 210
  210 ii = n
      go to 250
  220 if ( ch .lt. w(n) ) go to 230
      ch = ch - w(n)
      profit = profit + p(n)
      xx(n) = 1
  230 ii = n - 1
      go to 250
  240 ii = ll + 2
      if ( ch .ge. min(ii-1) ) go to 80
c
c save the current optimal solution.
c
  250 if ( z .ge. profit ) go to 270
      z = profit
      do j=1,n
        x(j) = xx(j)
      end do
      if ( z .eq. lim ) return
  270 if ( xx(n) .eq. 0 ) go to 280
      xx(n) = 0
      ch = ch + w(n)
      profit = profit - p(n)
c
c backtrack.
c
  280 nn = ii - 1
      if ( nn .eq. 0 ) return
      do j=1,nn
        kk = ii - j
        if ( xx(kk) .eq. 1 ) go to 300
      end do
      return
  300 r = ch
      ch = ch + w(kk)
      profit = profit - p(kk)
      xx(kk) = 0
      if ( r .lt. min(kk) ) go to 310
      ii = kk + 1
      go to 80
  310 nn = kk + 1
      ii = kk
c
c try to substitute the nn-th item for the kk-th.
c
  320 if ( z .ge. profit + ch*p(nn)/w(nn) ) go to 280
      diff = w(nn) - w(kk)
      if ( diff ) 370, 330, 340
  330 nn = nn + 1
      go to 320
  340 if ( diff .gt. r ) go to 330
      if ( z .ge. profit + p(nn) ) go to 330
      z = profit + p(nn)
      do j=1,kk
        x(j) = xx(j)
      end do
      jj = kk + 1
      do j=jj,n
        x(j) = 0
      end do
      x(nn) = 1
      if ( z .eq. lim ) return
      r = r - diff
      kk = nn
      nn = nn + 1
      go to 320
  370 t = r - diff
      if ( t .lt. min(nn) ) go to 330
      if ( z .ge. profit + p(nn) + t*p(nn+1)/w(nn+1)) go to 280
      ch = ch - w(nn)
      profit = profit + p(nn)
      xx(nn) = 1
      ii = nn + 1
      wsign(nn) = w(nn)
      psign(nn) = p(nn)
      zsign(nn) = ii
      n1 = nn + 1

      do j=n1,lold
        wsign(j) = 0
        psign(j) = 0
        zsign(j) = j
      end do

      lold = nn
      go to 80
      end
      subroutine mt1r ( n, p, w, c, eps, z, x, jdim, jck, xx, min,
     &  psign, wsign, zsign, crc, crp )

c*********************************************************************72
c
cc MT1R solves the 0-1 single knapsack problem with real parameters.
c
c maximize  z = p(1)*x(1) + ... + p(n)*x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) .le. c ,
c               x(j) = 0 or 1  for j=1,...,n.
c
c the program  implements the branch-and-bound algorithm described in
c section  2.5.2 .
c the program is a modified version of subroutine mt1.
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdim - 1 ;
c   2) p(j), w(j), c  positive reals;
c   3) max (w(j)) .le. c ;
c   4) w(1) + ... + w(n) .gt. c ;
c   5) p(j)/w(j) .ge. p(j+1)/w(j+1) for j=1,...,n-1.
c
c mt1r calls  1  procedure: chmt1r.
c
c the program is completely self-contained and communication to it is
c achieved solely through the parameter list of mt1r.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mt1r needs  10  arrays ( p ,  w ,  x ,  xx ,  min ,  psign ,  wsign ,
c                          zsign ,  crc  and  crp ) of length at least
c                          n + 1 .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c
c n    = number of items;
c p(j) = profit of item  j  (j=1,...,n);
c w(j) = weight of item  j  (j=1,...,n);
c c    = capacity of the knapsack;
c eps  = tolerance (two positive values q and r  are considered equal
c        if  abs(q-r)/max(q,r) .le. eps );
c jdim = dimension of the 10 arrays;
c jck  = 1 if check on the input data is desired,
c      = 0 otherwise.
c
c meaning of the output parameters:
c z    = value of the optimal solution if  z .gt. 0 ,
c      = error in the input data (when jck=1) if z .lt. 0 : condi-
c        tion  - z  is violated;
c x(j) = 1 if item  j  is in the optimal solution,
c      = 0 otherwise;
c
c arrays xx, min, psign, wsign, zsign, crc and crp are dummy.
c
c parameters n, x, jdim, jck, xx and zsign are integer. parameters p,
c w, c, z, min, psign, wsign, crc, crp and eps are real. on return of
c mt1r all the input parameters are unchanged.
c
cf2py intent(in) n, p, w, c, eps, jdim, jck
cf2py intent(hide) xx, min, psign, wsign, zsign, crc, crp
cf2py intent(out) z, x
cf2py depend(jdim) p, w, x, xx, zsign, min, psign, wsign, crc, crp
      real    p(jdim),w(jdim)
      integer x(jdim)
      integer xx(jdim),zsign(jdim)
      real    min(jdim),psign(jdim),wsign(jdim),crc(jdim),crp(jdim)
      real    lim,lim1,ip,mink,iu
      z = 0.
      if ( jck .eq. 1 ) call chmt1r(n,p,w,c,z,jdim)
      if ( z .lt. 0. ) return
c
c initialize.
c
      ip = 0.
      chs = c + eps*c
      do ll=1,n
        if ( w(ll) .gt. chs ) go to 20
        ip = ip + p(ll)
        chs = chs - w(ll)
      end do

   20 ll = ll - 1
      if ( chs .le. 0. ) go to 50
      p(n+1) = - 4.*ip*c
      w(n+1) = 2.*c
      lim = ip + chs*p(ll+2)/w(ll+2)
      a = w(ll+1) - chs
      b = ip + p(ll+1)
      lim1 = b - a*p(ll)/w(ll)
      if ( lim1 .gt. lim ) lim = lim1
      epsp = eps*ip
      lim = lim - epsp
      mink = 2.*c
      min(n) = mink
      do j=2,n
        kk = n + 2 - j
        if ( w(kk) .lt. mink ) mink = w(kk)
        min(kk-1) = mink
      end do
      do j=1,n
        xx(j) = 0
      end do
      ch = c + eps*c
      profit = 0.
      lold = n
      jj = 1
      nm2 = n - 2
      go to 180
   50 z = ip
      do j=1,ll
        x(j) = 1
      end do
      nn = ll + 1
      do j=nn,n
        x(j) = 0
      end do
      return
c
c try to insert the ii-th item into the current solution.
c
   80 jj = ii
      ch = crc(ii)
      profit = crp(ii)
   90 if ( w(jj) .le. ch ) go to 100
      jj1 = jj + 1
      if ( z + epsp .ge. ch*p(jj1)/w(jj1) + profit ) go to 290
      jj = jj1
      go to 90
c
c build a new current solution.
c
  100 ip = psign(jj)
      chs = ch - wsign(jj)
      in = zsign(jj)
      do 110 ll=in,n
        if ( w(ll) .gt. chs ) go to 170
        ip = ip + p(ll)
        chs = chs - w(ll)
  110 continue
      ll = n
  120 if ( z .ge. profit + ip ) go to 290
      z = profit + ip
      nn = jj - 1
      do 130 j=1,nn
        x(j) = xx(j)
  130 continue
      do 140 j=jj,ll
        x(j) = 1
  140 continue
      if ( ll .eq. n ) go to 160
      nn = ll + 1
      do 150 j=nn,n
        x(j) = 0
  150 continue
  160 if ( z .le. lim ) go to 290
      go to 400
  170 iu = chs*p(ll)/w(ll)
      ll = ll - 1
      if ( iu .le. epsp ) go to 120
      if ( z + epsp .ge. profit + ip + iu ) go to 290
c
c save the current solution.
c
  180 ii = jj
      crc(ii) = ch
      crp(ii) = profit
      crc(ii+1) = crc(ii) - w(ii)
      crp(ii+1) = crp(ii) + p(ii)
      nn = ll - 1
      j1 = ll + 1
      if ( nn .lt. ii) go to 200
      do 190 j=ii,nn
        jp1 = j + 1
        crc(j+2) = crc(jp1) - w(jp1)
        crp(j+2) = crp(jp1) + p(jp1)
  190 continue
  200 profit = crp(ll+1)
      do 210 j=j1,lold
        wsign(j) = 0.
        psign(j) = 0.
        zsign(j) = j
  210 continue
      lold = ll
      nel = ll - ii + 1
      do 220 jj=1,nel
        j = j1 - jj
        wsign(j) = wsign(j+1) + w(j)
        psign(j) = psign(j+1) + p(j)
        zsign(j) = j1
        xx(j) = 1
  220 continue
      if ( ll .le. nm2 ) go to 230
      ii = n
      go to 260
  230 crc(ll+2) = crc(ll+1)
      crp(ll+2) = crp(ll+1)
      if ( ll .lt. nm2 ) go to 250
      if ( crc(n) .lt. w(n) ) go to 240
      profit = profit + p(n)
      xx(n) = 1
  240 ii = n - 1
      go to 260
  250 ii = ll + 2
      if ( crc(ll+2) .ge. min(ii-1) ) go to 80
c
c save the current optimal solution.
c
  260 if ( z .ge. profit ) go to 280
      z = profit
      do 270 j=1,n
        x(j) = xx(j)
  270 continue
      if ( z .ge. lim ) go to 400
  280 xx(n) = 0
c
c backtrack.
c
  290 nn = ii - 1
      if ( nn .eq. 0 ) go to 400
      do 300 j=1,nn
        kk = ii - j
        if ( xx(kk) .eq. 1 ) go to 310
  300 continue
      go to 400
  310 r = crc(kk+1)
      crc(kk+1) = crc(kk)
      crp(kk+1) = crp(kk)
      ch = crc(kk)
      profit = crp(kk)
      xx(kk) = 0
      if ( r .lt. min(kk) ) go to 320
      ii = kk + 1
      go to 80
  320 nn = kk + 1
      ii = kk
c
c try to substitute the nn-th item for the kk-th.
c
  330 if ( z + epsp .ge. profit + ch*p(nn)/w(nn) ) go to 290
      diff = w(nn) - w(kk)
      if ( diff ) 380, 340, 350
  340 nn = nn + 1
      go to 330
  350 if ( diff .gt. r ) go to 340
      if ( z + epsp .ge. profit + p(nn) ) go to 340
      z = profit + p(nn)
      do 360 j=1,kk
        x(j) = xx(j)
  360 continue
      jj = kk + 1
      do 370 j=jj,n
        x(j) = 0
  370 continue
      x(nn) = 1
      if ( z .ge. lim ) go to 400
      r = ch - w(nn)
      kk = nn
      nn = nn + 1
      go to 330
  380 t = ch - w(nn)
      if ( t .lt. min(nn) ) go to 340
      if ( z + epsp .ge. profit + p(nn) + t*p(nn+1)/w(nn+1) ) go to 290
      crc(nn) = ch
      crp(nn) = profit
      crc(nn+1) = ch - w(nn)
      crp(nn+1) = profit + p(nn)
      xx(nn) = 1
      ii = nn + 1
      wsign(nn) = w(nn)
      psign(nn) = p(nn)
      zsign(nn) = ii
      n1 = nn + 1
      do 390 j=n1,lold
        wsign(j) = 0.
        psign(j) = 0.
        zsign(j) = j
  390 continue
      lold = nn
      go to 80
  400 return
      end
      subroutine mt2 ( n, p, w, c, z, x, jdim, jfo, jfs, jck, jub,
     &  ia1, ia2, ia3, ia4, ra )

c*********************************************************************72
c
cc MT2 solves the 0-1 single knapsack problem.
c
c maximize  z = p(1)*x(1) + ... + p(n)*x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) .le. c ,
c               x(j) = 0 or 1  for j=1,...,n.
c
c the program implements the enumerative algorithm described in
c section  2.9.3 .
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdim - 3 ;
c   2) p(j), w(j), c  positive integers;
c   3) max (w(j)) .le. c ;
c   4) w(1) + ... + w(n) .gt. c ;
c
c and, if  jfs = 1 ,
c
c   5) p(j)/w(j) .ge. p(j+1)/w(j+1) for j=1,...,n-1.
c
c mt2 calls  9  procedures: chmt2, core, cores, fmed, kp01m, newb,
c                           redns, reds and sortr.
c
c the program is completely self-contained and communication to it is
c achieved solely through the parameter list of mt2.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mt2 needs  8  arrays ( p ,  w ,  x ,  ia1 ,  ia2 ,  ia3 ,  ia4  and
c                        ra ) of length at least  n + 3 .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c
c n    = number of items;
c p(j) = profit of item  j  (j=1,...,n);
c w(j) = weight of item  j  (j=1,...,n);
c c    = capacity of the knapsack;
c jdim = dimension of the 8 arrays;
c jfo  = 1 if optimal solution is required,
c      = 0 if approximate solution is required;
c jfs  = 1 if the items are already sorted according
c          to decreasing profit per unit weight,
c      = 0 otherwise;
c jck  = 1 if check on the input data is desired,
c      = 0 otherwise.
c
c meaning of the output parameters:
c z    = value of the solution found if  z .gt. 0 ,
c      = error in the input data (when jck=1) if z .lt. 0 : condi-
c        tion  - z  is violated;
c x(j) = 1 if item  j  is in the solution found,
c      = 0 otherwise;
c jub  = upper bound on the optimal solution value (to evaluate  z
c        when jfo=0).
c
c arrays ia1, ia2, ia3, ia4 and ra are dummy.
c
c all the parameters but ra are integer. on return of mt2 all the
c input parameters are unchanged.
c
cf2py intent(in) n, p, w, c, jdim, jfo, jfs, jck
cf2py intent(hide) ia1, ia2, ia3, ia4, ra
cf2py intent(out) z, x, jub
Cf2py depend(jdim) p, w, x, ia1, ia2, ia3, ia4, ra
      integer p(jdim),w(jdim),x(jdim),c,z
      integer ia1(jdim),ia2(jdim),ia3(jdim),ia4(jdim)
      real    ra(jdim)
      integer fn1,fn0
      z = 0
      jub = 0
      if ( jck .eq. 1 ) call chmt2(n,p,w,c,jfs,z,jdim)
      if ( z .lt. 0 ) return
c
c step 1 (initialization and definition of the core problem).
c
      jds = jdim/3
      np1 = n + 1
c
c on return of core or cores, assuming that n1 (n0) is the set of items
c temporarily included in (excluded from) the solution, we have:
c iz1 = total profit of items in set n1;
c icw = c - total weight of items in set n1;
c minw0 = minimum weight of items in set n0;
c ia2(1) to ia2(nnf) = items in the core problem (free items);
c ia1(i) = successor of item i in sets n0 and n1 (= n + 1 if last);
c fn1 = pointer to the first item in set n1 (= n + 1 if empty);
c fn0 = pointer to the first item in set n0 (= n + 1 if empty);
c ra(i) = p(i)/w(i).
c
      if ( jfs .eq. 0 ) go to 10
      call cores(n,p,w,c,jfo,iz1,icw,minw0,jdim,ia2,nnf,ia1,fn1,fn0,ra)
      go to 20
   10 call core(n,p,w,c,jfo,iz1,icw,minw0,jdim,ia2,nnf,ia1,fn1,fn0,ra)
   20 if ( nnf .eq. n ) go to 130
      if ( nnf .gt. jds ) go to 130
c
c step 2 (heuristic solution through the core problem).
c
c sort the items in the core problem.
c
      call sortr(nnf,ra,ia2,jdim)
c
c solve the core problem, through kp01m, with:
c   ia3(1)  to  ia3(nnf) = sorted profits of free items;
c   ia3(nnf+2)  to  ia3(2*nnf+1) = dummy for ps;
c   ia4(1)  to  ia4(nnf) = sorted weights of free items;
c   ia4(nnf+2)  to  ia4(2*nnf+1) = dummy for ws;
c   ia2(1)  to  ia2(nnf) = pointers to free items; on input they are
c                          .gt. 0 ; on output they are .gt. 0 (.lt. 0)
c                          if the corresponding item is set to 1 (to 0);
c   ia2(nnf+2)  to  ia2(2*nnf+1) = dummy for zs;
c   ia2(2*nnf+2)  to  ia2(3*nnf+1) = dummy for xx;
c   ra(1)  to  ra(nnf) = dummy for minw;
c   izc (on output) = solution value of the core problem;
c   iubf0 (on output) = upper bound on the problem defined by the free
c                       items and the items in n0.
      np2 = nnf + 2
      n2p2 = 2*nnf + 2
      n2p1 = 2*nnf + 1
      do i=1,nnf
        j = ia2(i)
        ia3(i) = p(j)
        ia4(i) = w(j)
      end do
      izc = 0
      iubf0 = 0
      call kp01m(nnf,ia3,ia4,icw,minw0,izc,ia2,jfo,iubf0,nnf+1,
     &           ia2(n2p2),ia3(np2),ia4(np2),ia2(np2),ra)
c
c jub = upper bound on the original problem.
c
      jub = iz1 + iubf0
c
c define the heuristic solution of value iz1 + izc.
c
      z = iz1 + izc
      icwr = icw
      jval = 1
      j = fn1
   40 if ( j .gt. n ) go to 50
      x(j) = jval
      j = ia1(j)
      go to 40
   50 if ( jval .eq. 0 ) go to 60
      jval = 0
      j = fn0
      go to 40

   60 continue

      do 80 i=1,nnf
        j = ia2(i)
        if ( j .lt. 0 ) go to 70
        x(j) = 1
        icwr = icwr - w(j)
        go to 80
   70   mj = - j
        x(mj) = 0
        ia2(i) = mj
   80 continue
      if ( icwr .lt. minw0 ) go to 110
c
c the solution is not maximal.
c
      j = fn0
   90 if ( j .gt. n ) go to 110
      if ( w(j) .gt. icwr ) go to 100
      x(j) = 1
      z = z + p(j)
      icwr = icwr - w(j)
      if ( icwr .lt. minw0 ) go to 110
  100 j = ia1(j)
      go to 90
c
c halting test.
c
  110 if ( z .eq. jub ) go to 280
      if ( jfo .eq. 0 ) go to 280
c
c step 3 (reduction, without sorting, of the items not in core).
c
      nnfo = nnf
c
c on return of redns, assuming that n1 (n0) is the set of items included
c in (excluded from) the solution, the meaning of the parameters is the
c same given at step 1.
c
      call redns(n,p,w,izc,iz1,icw,ia2,nnfo,nnf,ia1,fn1,fn0)
c
c halting test.
c
      if ( nnf .eq. nnfo ) go to 280
c
c items previously fixed are now free. sort the free items.
c
      izn = z - iz1 - 1
      do 120 i=1,nnf
        j = ia2(i)
        ra(j) = float(p(j))/float(w(j))
        x(j) = 2
  120 continue
      call sortr(nnf,ra,ia2,jdim)
      go to 160
c
c step 4 (reduction, with preliminar sorting, of the original problem).
c
c sort the items in the original problem and define:
c   ia1(1) to ia1(n) = pointers to the original items;
c   ia3(1) to ia3(n) = sorted profits;
c   ia4(1) to ia4(n) = sorted weights.
c
  130 do 140 j=1,n
        ia1(j) = j
  140 continue
      if ( jfs .eq. 0 ) call sortr(n,ra,ia1,jdim)
      do 150 i=1,n
        j = ia1(i)
        ia3(i) = p(j)
        ia4(i) = w(j)
  150 continue
c
c on return of reds:
c x(j) = 0, 1 or 2 for item j fixed to 0, to 1 or free;
c ia2(1) to ia2(nnf) = pointers to free items;
c izh = heuristic solution;
c icw = c - total weight of items fixed to 1;
c iz1 = total profit of items fixed to 1.
c
      call reds(n,ia3,ia4,p,w,c,ia1,np1,nnf,x,iz1,izh,icw,ia2)
      izn = izh - iz1 - 1
c
c halting test.
c
      if ( nnf .gt. 0 ) go to 160
      z = izh
      go to 280
  160 if ( nnf .gt. jds ) go to 200
c
c step 5 (exact solution of the reduced problem if nnf is small).
c
c solve the reduced problem, through kp01m, with:
c   ia3(1) to ia3(nnf) = sorted profits of free items;
c   ia3(nnf+2) to ia3(2*nnf+1) = dummy for ps;
c   ia4(1) to ia4(nnf) = sorted weights of free items;
c   ia4(nnf+2) to ia4(2*nnf+1) = dummy for ws;
c   ia2(1) to ia2(nnf) = pointers to free items; on input they are
c                        .gt. 0 ; on output they are .gt. 0 (.lt. 0)
c                        if the corresponding item is set to 1 (to 0);
c   ia2(nnf+2) to ia2(2*nnf+1) = dummy for zs;
c   ia2(2*nnf+2) to ia2(3*nnf+1) = dummy for xx;
c   ra(1) to ra(nnf) = dummy for minw;
c   izn = solution of the reduced problem.
      np2 = nnf + 2
      n2p2 = 2*nnf + 2
      n2p1 = 2*nnf + 1
      do 170 i=1,nnf
        j = ia2(i)
        ia3(i) = p(j)
        ia4(i) = w(j)
        ia2(i) = j
  170 continue
      iubf0 = - 1
      call kp01m(nnf,ia3,ia4,icw,c+1,izn,ia2,jfo,iubf0,nnf+1,
     &           ia2(n2p2),ia3(np2),ia4(np2),ia2(np2),ra)
c
c define the optimal solution.
c
      z = iz1 + izn
      do 190 i=1,nnf
        j = ia2(i)
        if ( j .lt. 0 ) go to 180
        x(j) = 1
        go to 190
  180   mj = - j
        x(mj) = 0
  190 continue
      go to 280
c
c step 6 (exact solution of the reduced problem if nnf is large).
c
c solve the reduced problem, through kp01m, with:
c   ia3(1) to ia3(nnf) = sorted profits of free items;
c   ia3(nnf+2) to ia3(np1) = profits of fixed items;
c   ia4(1) to ia4(nnf) = sorted weights of free items;
c   ia4(nnf+2) to ia4(np1) = weights of fixed items;
c   ia2 = pointers to the original items;
c         ia2(1) to ia2(nnf) correspond to free items; on input they
c                              are .gt. 0 ; on output they are .gt. 0
c                              (.lt. 0) if the corresponding item is
c                              set to 1 (to 0);
c         ia2(nnf+1) to ia2(n) correspond to fixed items; they are
c                               .gt. 0 (.lt. 0) if the corresponding
c                               item is fixed to 1 (to 0);
c   p(1) to p(nnf) = dummy for xx;
c   w(1) to w(nnf) = dummy for ps;
c   x(1) to x(nnf) = dummy for ws;
c   ia1(1) to ia1(nnf) = dummy for zs;
c   ra(1) to ra(nnf) = dummy for minw;
c   izn = solution of the reduced problem.
c
  200 jl = n + 2
      do 220 j=1,n
        if ( x(j) .eq. 2 ) go to 220
        jl = jl - 1
        ia3(jl) = p(j)
        ia4(jl) = w(j)
        if ( x(j) .eq. 0 ) go to 210
        ia2(jl-1) = j
        go to 220
  210   ia2(jl-1) = - j
  220 continue
      do 230 i=1,nnf
        j = ia2(i)
        ia3(i) = p(j)
        ia4(i) = w(j)
  230 continue
      iubf0 = - 1
      call kp01m(nnf,ia3,ia4,icw,icw+1,izn,ia2,jfo,iubf0,nnf+1,
     &           p,w,x,ia1,ra)
c
c reset the original problem and define the optimal solution.
c
      z = iz1 + izn
      do 270 i=1,n
        j = ia2(i)
        if ( j .lt. 0 ) go to 240
        x(j) = 1
        go to 250
  240   j = - j
        x(j) = 0
  250   if ( i .gt. nnf ) go to 260
        p(j) = ia3(i)
        w(j) = ia4(i)
        go to 270
  260   p(j) = ia3(i+1)
        w(j) = ia4(i+1)
  270 continue
  280 if ( jub .eq. 0 ) jub = z
      return
      end
      subroutine mtb2(n,p,w,b,c,z,x,jdim1,jdim2,jfo,jfs,jck,jub,
     &                id1,id2,id3,id4,id5,id6,id7,rd8)

c*********************************************************************72
c
cc MTB2 solves the bounded single knapsack problem
c
c maximize  z = p(1)*x(1) + ... + p(n)*x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) .le. c ,
c               0 .le. x(j) .le. b(j) for j=1,...,n,
c               x(j)  integer         for j=1,...,n.
c
c the program implements the transformation method described in
c section  3.2 .
c
c the problem is transformed into an equivalent 0-1 knapsack
c problem and then solved through subroutine mt2. the user
c must link mt2 and its subroutines to this program.
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdim1 - 1 ;
c   2) p(j), w(j), b(j), c  positive integers;
c   3) max (b(j)*w(j)) .le. c ;
c   4) b(1)*w(1) + ... + b(n)*w(n) .gt. c ;
c   5) 2 .le. n + ( log2(b(1)) + ... + log2(b(n)) ) .le. jdim2 - 3 ;
c
c and, if  jfs = 1 ,
c
c   6) p(j)/w(j) .ge. p(j+1)/w(j+1) for j=1,...,n-1.
c
c mtb2 calls  4  procedures: chmtb2, sol, trans and mt2 (external).
c
c communication to the program is achieved solely through the parameter
c list of mtb2.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mtb2 needs
c   4  arrays ( p ,  w ,  b  and  x ) of length at least  jdim1 ;
c   8  arrays ( id1 ,  id2 ,  id3 ,  id4 ,  id5 ,  id6 ,  id7  and
c               rd8 ) of length at least  jdim2 .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c n     = number of item types;
c p(j)  = profit of each item of type  j  (j=1,...,n);
c w(j)  = weight of each item of type  j  (j=1,...,n);
c b(j)  = number of items of type  j  available  (j=1,...,n);
c c     = capacity of the knapsack;
c jdim1 = dimension of p, w, b, x;
c jdim2 = dimension of id1, id2, id3, id4, id5, id6, id7, rd8.
c jfo   = 1 if optimal solution is required,
c       = 0 if approximate solution is required;
c jfs   = 1 if the items are already sorted according to
c           decreasing profit per unit weight (suggested
c           for large  b(j)  values),
c       = 0 otherwise;
c jck   = 1 if check on the input data is desired,
c       = 0 otherwise;
c
c meaning of the output parameters:
c z    = value of the solution found if  z .gt. 0 ,
c      = error in the input data (when jck=1) if z .lt. 0 : condi-
c        tion  - z  is violated;
c x(j) = number of items of type  j  in the solution found;
c jub  = upper bound on the optimal solution value (to evaluate z
c        when jfo=0).
c
c arrays id1, id2, id3, id4, id5, id6, id7 and rd8 are dummy.
c
c all the parameters but rd8 are integer. on return of mtb2 all the
c input parameters are unchanged.
c
cf2py intent(in) n, p, w, b, c, jdim1, jdim2, jfo, jfs, jck
cf2py intent(hide) id1, id2, id3, id4, id5, id6, id7, rd8
cf2py intent(out) z, x, jub
cf2py depend(jdim1) p, w, b, x
cf2py depend(jdim2) id1, id2, id3, id4, id5, id6, id7, rd8
      integer p(jdim1),w(jdim1),b(jdim1),x(jdim1),c,z
      integer id1(jdim2),id2(jdim2),id3(jdim2),id4(jdim2),id5(jdim2),
     &        id6(jdim2),id7(jdim2)
      real    rd8(jdim2)
      z = 0
      if ( jck .eq. 1 ) call chmtb2(n,p,w,b,c,jfs,z,jdim1)
      if ( z .lt. 0 ) return
c
c transform the bounded knapsack problem into an equivalent
c 0-1 knapsack problem.
c
      call trans(n,p,w,b,jdim1,jdim2,nt,id1,id2)
      if ( nt .gt. 0 ) go to 10
      z = - 5
      return
c
c solve the equivalent 0-1 knapsack problem.
c
   10 call mt2(nt,id1,id2,c,z,id3,jdim2,jfo,jfs,0,jub,
     &         id4,id5,id6,id7,rd8)
c
c determine the solution vector for the original problem.
c
      call sol(n,b,id3,jdim1,jdim2,x)
      return
      end
      subroutine mtc1(n,w,c,lb,z,x,jdn,jdl,maxbck,xx,m,l)

c*********************************************************************72
c
cc MTC1 solves a change-making problem through the branch-and-bound algorithm.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    "optimal and canonical solutions of the change-making problem",
c    european journal of operational research,
c    1980.
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdn),x(jdn),c,z
      integer xx(jdn),cwf,s,profit
      integer m(jdl),l(jdl)

      kbck = 0
      cwf = c
      w(n+1) = 1
      jdom = jdl
      if ( jdom .ge. w(1) ) jdom = w(1) - 1
      if ( jdom .lt. w(n) ) go to 30
      n2 = n + 2

      do jj=2,n
        j = n2 - jj
        k1 = w(j)
        k2 = w(j-1) - 1
        if ( k2 .gt. jdom ) k2 = jdom
        do k=k1,k2
          m(k) = j
          l(k) = 0
        end do
        if ( k2 .eq. jdom ) go to 30
      end do

   30 k1 = w(n) - 1
      if ( k1 .gt. jdom ) k1 = jdom
      if ( k1 .eq. 0 ) go to 50
      do k=1,k1
        l(k) = z
      end do
   50 xx(1) = c/w(1)
      do 60 j=2,n
        xx(j) = 0
   60 continue
      profit = xx(1)
      c = c - xx(1)*w(1)
      ii = 2
c
c step (2.a).
c
   70 if ( c .le. jdom ) go to 90
      if ( c .lt. w(n) ) go to 150
      iiold = ii
   80 if ( c .ge. w(ii) ) go to 100
      ii = ii + 1
      go to 80
   90 if ( l(c) .ge. z - profit ) go to 150
      iiold = ii
      ii = m(c)
c
c step 2.
c
  100 s = c/w(ii)
      ict = c - s*w(ii)
      if ( z .le. profit + s + (ict + w(ii+1) - 1)/w(ii+1) ) go to 110
      if ( ict .eq. 0 ) go to 130
      if ( ii .lt. n ) go to 120
  110 ii = iiold
      go to 150
c
c step 3.
c
  120 c = ict
      profit = profit + s
      xx(ii) = s
      ii = ii + 1
      go to 70
c
c step 4.
c
  130 z = profit + s
      do 140 j=1,n
        x(j) = xx(j)
  140 continue
      x(ii) = s
      if ( z .ne. lb ) go to 150
      c = cwf
      return
c
c step 5.
c
  150 kbck = kbck + 1
      if ( kbck .eq. maxbck ) go to 170
      ib = ii - 1
      do 160 j=1,ib
        iimj = ii - j
        if ( xx(iimj) .gt. 0 ) go to 180
  160 continue
  170 c = cwf
      if ( z .gt. c ) z = 0
      return
  180 kk = ii - j
      if ( c .ge. w(kk) ) go to 190
      if ( c .gt. jdom ) go to 190
      if ( z - profit .gt. l(c) ) l(c) = z - profit
  190 c = c + w(kk)
      profit = profit - 1
      xx(kk) = xx(kk) - 1
      if ( z .gt. profit + (c + w(kk+1) - 1)/w(kk+1) ) go to 200
      c = c + xx(kk)*w(kk)
      profit = profit - xx(kk)
      xx(kk) = 0
      ii = kk + 1
      go to 150
  200 ii = kk + 1
      iiold = ii
      if ( c .gt. jdom ) go to 210
      if ( l(c) .ge. z - profit ) go to 150
  210 if ( c - w(kk) .ge. w(n) ) go to 100
      ih = kk
c
c step 6.
c
  220 ih = ih + 1
      if ( z .le. profit + (c + w(ih) - 1)/w(ih) ) go to 150
      if ( ih .gt. n ) go to 150
      if ( c - w(ih) .lt. w(n) ) go to 220
      ii = ih
      iiold = ii
      go to 100
      end
      subroutine mtc2 ( n, w, c, z, x, jdn, jdl, jfo, back, jck, xx, wr, 
     & pr, m, l )

c*********************************************************************72
c
cc MTC2 solves the unbounded change-making problem
c
c minimize  z = x(1) + ... + x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) = c ,
c               x(j) .ge. 0 and integer  for j=1,...,n.
c
c the program implements the enumerative algorithm described in
c section  5.6 .
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdn - 1 ;
c   2) w(j), c  positive integers;
c   3) max (w(j)) .lt. c .
c
c mtc2 calls  5  procedures: chmtc2, corec, maxt, mtc1 and sorti.
c
c the program is completely self-contained and communication to it is
c achieved solely through the parameter list of mtc2.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mtc2 needs
c   5  arrays ( w ,  x ,  xx ,  wr  and  pr ) of length at least
c               jdn ;
c   2  arrays ( m  and  l ) of length at least  jdl .
c
c  Modified:
c
c    06 December 2009
c    27 July 2022 (jmyrberg)
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c
c n     = number of item types;
c
c w(j)  = weight of each item of type  j  (j=1,...,n);
c
c c     = capacity;
c
c jdn   = dimension of arrays  w ,  x ,  xx ,  wr  and  pr ;
c
c jdl   = dimension of arrays  m  and  l ( suggested value
c                   jdl = max (w(j)) - 1 ;
c         if the core memory is not enough,  jdl  should be set
c         to the largest possible value);
c
c jfo   = 1 if optimal solution is required,
c       = 0 if approximate solution is required (at most back
c         backtrackings are performed);
c
c back  = maximum number of backtracks to perform when jfo = 0
c
c jck   = 1 if check on the input data is desired,
c       = 0 otherwise.
c
c meaning of the output parameters:
c
c z    = value of the solution found if  z .gt. 0 ,
c      = no feasible solution exists if z .eq. 0 ,
c      = error in the input data (when jck=1) if z .lt. 0 : condi-
c        tion  - z  is violated;
c
c x(j) = number of items of type  j  in the solution found.
c
c arrays xx, m, l, wr and pr are dummy.
c
c all the parameters are integer. on return of mtc2 all the input
c parameters are unchanged.
c
cf2py intent(in) n, w, c, jdn, jdl, jfo, back, jck
cf2py intent(hide) xx, m, l, wr, pr
cf2py intent(out) z, x
cf2py depend(jdn)  w, x, xx, wr, pr
cf2py depend(jdl)  m, l
      integer w(jdn),x(jdn),c,z,back
      integer xx(jdn),wr(jdn),pr(jdn)
      integer m(jdl),l(jdl)
      integer prj,cws,s1,s2

      z = c + 1

      if ( jck .eq. 1 ) then
        call chmtc2(n,w,c,z,jdn)
      end if

      if ( z .lt. 0 ) then
        return
      end if

      maxbck = - 1

      if ( jfo .eq. 0 ) then
        maxbck = back
      end if
c
c lower bound computation.
c
      call maxt(n,w,i1,i2,i3,jdn)

      s1 = c/w(i1)
      s2 = (c - s1*w(i1))/w(i2)
      ip = s1 + s2
      cws = c - s1*w(i1) - s2*w(i2)

      if ( cws .eq. 0 ) then

        z = ip
        do j=1,n
          x(j) = 0
        end do
        x(i1) = s1
        x(i2) = s2
        return

      end if

      lb = ip + (cws + w(i3) - 1)/w(i3)
      l1 = ip - 1 + (cws + w(i1) + w(i2) - 1)/w(i2)

      if ( l1 .lt. lb ) then
        lb = l1
      end if

      if ( n .le. 500 ) then
        go to 90
      end if
c
c determine and solve the core problem.
c
      call corec(n,w,i1,i2,i3,jdn,nc,pr)
      call sorti(nc,w,pr,jdn)

      do j=1,nc
        prj = pr(j)
        wr(j) = w(prj)
      end do

      call mtc1(nc,wr,c,lb,z,xx,jdn,jdl,maxbck,x,m,l)

      if ( z .gt. 0 ) go to 40
      z = c + 1
      go to 90

   40 continue

      do j=1,n
        x(j) = 0
      end do
      do j=1,nc
        prj = pr(j)
        x(prj) = xx(j)
      end do
c
c the core problem solution is optimal.
c
      if ( z .eq. lb ) then
        return
      end if

      do j=1,n
        xx(j) = x(j)
      end do
c
c solve the complete problem.
c
   90 continue

      do j=1,n
        pr(j) = j
      end do

      call sorti(n,w,pr,jdn)

      do j=1,n
        prj = pr(j)
        wr(j) = w(prj)
      end do

      izh = z

      call mtc1(n,wr,c,lb,z,xx,jdn,jdl,maxbck,x,m,l)

      if ( z .eq. 0 ) then
        return
      end if
c
c store in x the final solution.
c
      if ( z .eq. izh ) then

        do j=1,n
          x(j) = xx(j)
        end do

      else

        do j=1,n
          prj = pr(j)
          x(prj) = xx(j)
        end do

      end if

      return
      end
      subroutine mtcb(n,w,b,c,z,x,jdn,jdl,jfo,back,jck,xx,wr,br,pr,m,l)

c*********************************************************************72
c
cc MTCB solves the bounded change-making problem
c
c minimize  z = x(1) + ... + x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) = c ,
c               0 .le. x(j) .le. 0 for j=1,...,n,
c               x(j) integer       for j=1,...,n.
c
c the program implements the branch-and-bound algorithm described
c in section  5.8 .
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdn - 1 ;
c   2) w(j), b(j), c  positive integers;
c   3) max (w(j)) .lt. c ;
c   4) b(j)*w(j) .le. c for j=1,...,n;
c   5) b(1)*w(1) + ...+ b(n)*w(n) .gt. c .
c
c mtcb calls  3  procedures: chmtcb, cmpb and sorti.
c
c the program is completely self-contained and communication to it is
c achieved solely through the parameter list of mtcb.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mtcb needs
c   7  arrays ( w ,  b ,  x ,  xx ,  wr ,  br  and  pr ) of length
c               at least  jdn ;
c   2  arrays ( m  and  l ) of length at least  jdl .
c
c  Modified:
c
c    06 December 2009
c    27 July 2022 (jmyrberg)
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c n     = number of item types;
c w(j)  = weight of each item of type  j  (j=1,...,n);
c b(j)  = number of available items of type j  (j=1,...,n);
c c     = capacity;
c jdn   = dimension of arrays  w ,  b ,  x ,  xx ,  wr ,  br  and  pr ;
c jdl   = dimension of arrays  m  and  l (suggested value
c                   jdl = max (w(j)) - 1 ;
c         if the core memory is not enough,  jdl  should be set
c         to the largest possible value);
c jfo   = 1 if optimal solution is required,
c       = 0 if approximate solution is required (at most 100000
c back  = maximum number of backtracks to perform when jfo = 0
c         backtrackings are performed);
c jck   = 1 if check on the input data is desired,
c       = 0 otherwise.
c
c meaning of the output parameters:
c z    = value of the solution found if  z .gt. 0 ,
c      = no feasible solution exists if z .eq. 0 ,
c      = error in the input data (when jck=1) if z .lt. 0 : condi-
c        tion  - z  is violated;
c x(j) = number of items of type  j  in the solution found.
c
c arrays xx, m, l, wr, br and pr are dummy.
c
c all the parameters are integer. on return of mtcb all the input
c parameters are unchanged.
c
cf2py intent(in) n, w, b, c, jdn, jdl, jfo, back, jck
cf2py intent(hide) xx, m, l, wr, br, pr
cf2py intent(out) z, x
cf2py depend(jdn)  w, b, x, xx, wr, br, pr
cf2py depend(jdl)  m, l
      integer w(jdn),b(jdn),x(jdn),c,z,back
      integer xx(jdn),wr(jdn),br(jdn),pr(jdn)
      integer m(jdl),l(jdl)
      z = c + 1
      if ( jck .eq. 1 ) call chmtcb(n,w,b,c,z,jdn)
      if ( z .lt. 0 ) return
      maxbck = - 1
      if ( jfo .eq. 0 ) maxbck = back
c
c sorting.
c
      do j=1,n
        pr(j) = j
      end do
      call sorti(n,w,pr,jdn)
      do j=1,n
        jpr = pr(j)
        wr(j) = w(jpr)
        br(j) = b(jpr)
      end do
c
c solution.
c
      call cmpb(n,wr,br,c,z,xx,jdn,jdl,maxbck,x,m,l)
c
c store in x the final solution.
c
      do j=1,n
        jpr  = pr(j)
        x(jpr) = xx(j)
      end do

      return
      end
      subroutine mtg ( n, m, p, w, c, minmax, z, xstar, back, jck, jb )

c*********************************************************************72
c
cc MTG solves the generalized assignment problem
c
c opt z = p(1,1)*x(1,1) + ... + p(1,n)*x(1,n) +
c                         ...                 +
c         p(m,1)*x(m,1) + ... + p(m,n)*x(m,n)
c
c     (where  opt = min  if  minmax = 1 ,  opt = max  if  minmax = 2 )
c
c subject to:
c
c       w(i,1)*x(i,1) + ... + w(i,n)*x(i,n) .le. c(i)  for i=1,...,m,
c       x(1,j) + ... + x(m,j) = 1                      for j=1,...,n,
c       x(i,j) = 0 or 1                     for i=1,...,m, j=1,...,n.
c
c the program implements the branch-and-bound algorithm described
c in sections  7.3 - 7.5 .
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. m .le. jdimr ;
c   2) 2 .le. n .le. jdimc
c      ( jdimr  and  jdimc  are defined by the first two executable
c       statements);
c   3) m .le. jdimpc
c      ( jdimpc , defined by the third executable statement, is used
c       for packing array  y , and cannot be greater than
c       (number of bits of the host) - 2 ; if a higher value is
c       desired, subroutines ydef and yuse must be re-structured
c       accordingly);
c   4) p(i,j), w(i,j) and c(i) positive integers;
c   5) w(i,j) .le. c(i) for at least one i, for j=1,...,n;
c   6) c(i) .ge. min (w(i,j)) for i=1,...,m.
c
c in addition, it is required that
c
c   7) (maximum level of the decision-tree) .le. jnlev .
c      ( jnlev  is defined by the fourth executable statement.)
c
c mtg calls 24 procedures: chmtg, defpck, dmind, feas, gha, ghbcd,
c                          ghx, gr1, gr2, heur, kpmax, kpmin, pen0,
c                          pen1, prepen, skp1, sorti, sortr, termin,
c                          trin, ubfjv, ubrs, ydef and yuse.
c if not present in the library of the host, the user must supply an
c integer function  iand ( i1, i2 )  which sets  iand to the bit-by-bit
c logical and of  i1  and  i2 . such function is used in subroutines
c ydef and yuse.
c
c communication to the program is  achieved solely through the
c parameter list of mtg.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mtg needs
c   17 arrays ( c ,  dd ,  ud ,  q ,  pakl ,  ip ,  ir ,  il ,  if ,
c               wobbl ,  kq ,  flrep ,  dmyr1 ,  dmyr2 ,  dmyr3 ,
c               dmyr4  and  dmyr5 )  of length at least  m ;
c   25 arrays ( xstar ,  xs ,  bs ,  b ,  ka ,  xxs ,  iobbl ,  jobbl ,
c               best ,  xjjub ,  ds ,  dmyc1 ,  dmyc2 ,  dmyc3 ,
c               dmyc4 ,  dmyc5 ,  dmyc6 ,  dmyc7 ,  dmyc8 ,  dmyc9 ,
c               dmyc10 ,  dmyc11 ,  dmyc12 , dmyc13  and  dmycr1 ) of
c               length at least  n ;
c    4 arrays ( ps ,  ws ,  dmycc1  and  dmycc2 )  of length at least
c               n + 1 ;
c    6 arrays ( e ,  cc ,  cs ,  type ,  us  and  ubl ) of length at
c               least  jnlev ;
c    7 arrays ( p ,  w ,  a ,  x ,  pak ,  kap  and  mind ) of length
c               at least  m x n ;
c    5 arrays ( d ,  vs ,  v ,  lb  and  ub )  of length at least
c               jnlev x m ;
c    1 array  ( y )  of length at least  jnlev x n ;
c    2 arrays ( mask1  and  itwo ) of length at least  jdimpc .
c
c the arrays are currently dimensioned to allow problems for which
c       m .le. 10 ,
c       n .le. 100 ,
c   jnlev .le. 150 ,
c on a 32-bit computer (so, in the calling program, arrays  p  and  w
c must be dimensioned at  (10,100) ). changing such limits necessitates
c changing the dimensions of all the arrays in subroutine mtg and in
c common /pack/ (which is included in subroutines mtg, ydef and yuse),
c as well as the four first executable statements.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c
c n        = number of items;
c
c m        = number of knapsacks;
c
c p(i,j)   = profit of item  j  if assigned to knapsack  i
c            (i=1,...,m; j=1,...,n);
c w(i,j)   = weight of item  j  if assigned to knapsack  i
c            (i=1,...,m; j=1,...,n);
c c(i)     = capacity of knapsack  i  (i=1,...,m);
c minmax   = 1 if the objective function must be minimized,
c          = 2 if the objective function must be maximized;
c back     = - 1 if exact solution is required,
c          = maximum number of backtrackings to be performed,
c            if heuristic solution is required;
c jck      = 1 if check on the input data is desired,
c          = 0 otherwise.
c
c meaning of the output parameters:
c z        = value of the optimal solution if z .gt. 0 ,
c          = 0 if no feasible solution exists,
c          = error in the input data (when jck=1) if z .lt. 0 : condi-
c            tion  - z  is violated;
c xstar(j) = knapsack where item  j  is inserted in the solution found;
c jb       = lower bound (if minmax=1) or upper bound (if minmax=2)
c            on the optimal solution value.
c
c all the parameters are integer. on return of mtg all the input
c parameters are unchanged, with the following two exceptions.  back
c gives the number of backtrackings performed;  p(i,j)  is set to  0
c for all pairs  (i,j)  such that  w(i,j) .gt. c(i) .
c
c meaning of the main internal variables:
c b(j)   = 0 if the assignment of item  j  is fixed,
c        = 1 otherwise;
c a(i,j) =   1 if assignment of item  j  to knapsack  i  is fixed,
c        = - 1 if assignment of item  j  to knapsack  i  is prohibited,
c        =   0 otherwise;
c x(i,j) =   current solution of the relaxed problem;
c y(l,j) =   packed solution of the relaxed problem for item  j  at
c            level  l  of the branch-decision tree.
c
cf2py intent(in) n, m, p, w, c, minmax, back, jck
cf2py intent(out) z, xstar, jb
      integer       p(10,100),w(10,100),c(10),xstar(100),z,back
      integer       h,s,r,u,su,vc,sb,t,qh,zbound,vjjub,penalt
      integer       dd(10),ud(10),q(10),pakl(10),ip(10),ir(10),il(10),
     &              if(10),wobbl(10),kq(10),flrep(10)
      integer       xs(100),bs(100),b(100),ka(100),xxs(100),iobbl(100),
     &              jobbl(100),best(100),xjjub(100)
      real          ds(100)
      integer       ps(101),ws(101)
      integer       e(150),cc(150),cs(150),type(150),us(150),ubl(150)
      integer       a(10,100),x(10,100),pak(10,100),kap(10,100),
     &              mind(10,100)
      integer       d(150,10),vs(150,10),v(150,10),lb(150,10),ub(150,10)
      integer       y
      integer       dmyr1(10),dmyr2(10),dmyr3(10),dmyr4(10),dmyr5(10)
      integer       dmyc1(100),dmyc2(100),dmyc3(100),dmyc4(100),
     &              dmyc5(100),dmyc6(100),dmyc7(100),dmyc8(100),
     &              dmyc9(100),dmyc10(100),dmyc11(100),dmyc12(100),
     &              dmyc13(100)
      integer       dmycc1(101),dmycc2(101)
      real          dmycr1(100)

      common /pack/ mask1(30),itwo(30),mask,y(150,100)
c
c definition of the internal parameters.
c
      jdimr = 10
      jdimc = 100
      jdimpc = 30
      jnlev = 150
      z = 0
      if ( jck .eq. 1 ) call chmtg(n,m,p,w,c,jdimr,jdimc,jdimpc,z)
      if ( z .lt. 0 ) return
c
c step 1 (initialize).
c
      numnod = 0
      invst = 0
      imult = - 1
      jb = 0
      if ( minmax .eq. 2 ) go to 10
c
c transform the minimization problem into a maximization problem.
c
      call trin(p,n,m,invst,lam,jdimr,jdimc)
      imult = 1
c
c solve the maximization problem.
c
c check for infeasibility.
c
   10 call feas(n,m,p,w,c,xstar,jfi,jdimr,jdimc)
      if ( jfi .eq. 1 ) go to 880
c
c heuristics
c
      call heur(p,w,c,n,m,z,xstar,iub,jub,best,kvst,inf,
     &         jdimr,jdimc,dmyr1,dmyr2,dmyr3,dmyr4,dmyr5,
     &         dmyc1,dmyc2,dmyc3,dmyc4,dmycr1,a)
      if ( z .ge. jub ) go to 880
      if ( back .eq. 0 ) go to 880
c
c first reduction.
c
      call gr1(p,w,c,n,m,z,xstar,iub,best,b,a,nr,kq,kvst,jdimr,jdimc)
      if ( nr .eq. 0 ) go to 880
      kub = iub
c
c define the vectors for packing y.
c
      call defpck(m,jdimpc)
c
c compute the initial martello-toth bound (su) for the root node.
c
      call dmind(n,m,p,w,mind,jdimr,jdimc,dmyc1,dmycr1)
      do i=1,m
        flrep(i) = 1
        q(i) = kq(i)
      end do
   30 su = 0
      do 130 i=1,m
        if ( flrep(i) .eq. 1 ) go to 40
        su = su + v(1,i)
        go to 130
   40   lb(1,i) = 1
        v(1,i) = 0
        kk = 0
        isum = 0
        jspr = 0
        do 60 jj=1,n
          j = mind(i,jj)
          xs(j) = 0
          if ( a(i,j) .eq. (- 1) ) go to 60
          if ( b(j) .eq. 1 ) go to 50
          xs(j) = 1
          jspr = jspr + p(i,j)
          go to 60
   50     if ( w(i,j) .gt. q(i) ) go to 60
          kk = kk + 1
          ws(kk) = w(i,j)
          isum = isum + ws(kk)
          ps(kk) = p(i,j)
          bs(kk) = j
   60   continue
        if ( isum .gt. q(i) ) go to 80
        if ( kk .eq. 0 ) go to 90
        do j=1,kk
          xxs(j) = 1
          v(1,i) = v(1,i) + ps(j)
        end do
        go to 90
   80   mubf = - 1
        call kpmax(kk,ps,ws,q(i),v(1,i),xxs,mubf,
     &             jdimc+1,jdimc,dmyc1,dmyc2,dmyc3,dmyc4,dmyc5)
   90   v(1,i) = v(1,i) + jspr
        su = su + v(1,i)
        if ( kk .eq. 0 ) go to 110
        do j=1,kk
          index = bs(j)
          xs(index) = xxs(j)
        end do
  110   do j=1,n
          x(i,j) = xs(j)
          call ydef(1,i,j,xs(j))
        end do
  130 continue
      l = 1
      cc(1) = 1
      d(1,1) = 1
      ub(1,1) = su
      jub = su
      if ( jub .gt. kub ) jub = kub
      if ( jub .le. z ) go to 880
      l = 2
c
c step 2 (forward step).
c
  140 if ( numnod .eq. back ) go to 880
      numnod = numnod + 1
      if ( numnod .gt. 1 ) go to 150
c
c compute the fisher-jaikumar-van wassenhove bound (jjub).
c
      call ubfjv(n,m,p,w,q,b,a,jjub,xjjub,vjjub,z,inf,
     &           jdimr,jdimc,jdimc+1,dmyc1,dmyc2,dmyc3,dmycc1,
     &           dmycc2,dmycr1,dmyc4,dmyc5,dmyc6,dmyc7,dmyc8,dmyc9,
     &           dmyc10,dmyc11,dmyr1,dmyr2,dmyr3,dmyc12,pak)
      go to 160
c
c compute the improved ross-soland bound (jjub).
c
  150 call ubrs(n,m,p,w,q,b,a,jjub,xjjub,vjjub,z,inf,
     &          jdimr,jdimc,jdimc+1,dmyr1,dmyc1,dmyc2,dmyc3,dmyc4,
     &          dmyc5,dmyc6,dmyc7,dmyc8,dmyc9,dmyc10,dmyc11,
     &          dmyc12,dmyc13,dmycc1,dmycc2,dmycr1)
  160 if ( jjub .le. z ) go to 690
      if ( jjub .ne. vjjub ) go to 180
      z = vjjub
      do j=1,n
        xstar(j) = xjjub(j)
      end do
      go to 680
  180 ubl(l-1) = jjub
      if ( l .lt. jnlev ) go to 190
      z = - 7
      go to 880
c
c search for implied assignments.
c
  190 kobbl = 0

      do i=1,m
        wobbl(i) = 0
      end do

      rz = 0
      do 280 s=1,n
        ka(s) = 1
        if ( b(s) .eq. 0 ) go to 280
        rptot = 0
        rp1 = 0
        ka(s) = 0
        kfeas = 0
        do 220 i=1,m
          rsum = float(p(i,s))/float(w(i,s))
          if ( x(i,s) .eq. 0 ) go to 210
          ka(s) = ka(s) + 1
          rp1 = rp1 + rsum
          rptot = rptot + rsum
          kfeas = kfeas + 1
          ifeas = i
          go to 220
  210     rptot = rptot + rsum
          if ( a(i,s) .eq. (- 1) ) go to 220
          if ( w(i,s) .gt. q(i) ) go to 220
          kfeas = kfeas + 1
          ifeas = i
  220   continue
        if ( kfeas .eq. 0 ) go to 690
        if ( kfeas .gt. 1 ) go to 240
        wobbl(ifeas) = wobbl(ifeas) + w(ifeas,s)
        if ( wobbl(ifeas) .gt. q(ifeas) ) go to 690
        if ( x(ifeas,s) .eq. 1 ) go to 230
        rz = inf
        j = s
        jt = 0
        go to 280
c
c item s must be inserted into knapsack ifeas.
c
  230   kobbl = kobbl + 1
        iobbl(kobbl) = ifeas
        jobbl(kobbl) = s
        go to 260
  240   if ( ka(s) - 1 ) 250,280,260
  250   ak = rptot/float(m)
        go to 270
  260   ak = rp1
  270   ds(s) = ak
        if ( ak .le. rz ) go to 280
        rz = ak
        j = s
        jt = ka(s)
  280 continue
      if ( rz .le. 0. ) go to 650
      if ( kobbl .eq. 0 ) go to 310
c
c kobbl particular forward steps.
c
      do 300 i=1,kobbl
        l1 = l - 1
        ik = iobbl(i)
        jk = jobbl(i)
        e(l) = jk
        cs(l) = 1
        d(l,1) = ik
        cc(l) = 1
        b(jk) = 0
        a(ik,jk) = 1
        q(ik) = q(ik) - w(ik,jk)
        index = cc(l1)
        index = d(l1,index)
        ub(l,ik) = ub(l1,index)
        ubl(l) = ubl(l1)
        type(l) = 1
        do ii=1,m
          lb(l,ii) = lb(l1,ii)
          v(l,ii) = v(l1,ii)
        end do
        l = l + 1
  300 continue
      go to 190
  310 if ( z .eq. kvst ) go to 390
c
c compute the penalities for the improved martello-toth bound.
c
      call prepen(n,m,p,w,q,b,a,mind,pak,kap,pakl,ip,ir,il,if,
     &            jdimr,jdimc)
      izmax = - inf
      rzmax = - inf
      zbound = 0
      index = cc(l-1)
      index = d(l-1,index)
      iub = ub(l-1,index)
      do 370 jj=1,n
        if ( ka(jj) .eq. 1 ) go to 370
        if ( ka(jj) .eq. 0 ) go to 320
        call pen1(jj,m,p,w,x,v,l-1,pak,kap,pakl,ip,ir,il,if,penalt,
     &            jfo,iub,z,jdimr,jdimc,jnlev)
        if ( jfo .eq. 0 ) go to 330
        izmax = inf
        rzmax = inf
        j = jj
        jt = ka(jj)
        go to 330
  320   call pen0(jj,m,p,w,q,a,v,l-1,pak,kap,pakl,ip,ir,il,if,
     &            penalt,jfo,iub,z,inf,jdimr,jdimc,jnlev)
        if ( jfo .gt. 1 ) go to 330
        izmax = inf
        rzmax = inf
        j = jj
        jt = 0
  330   if ( penalt .le. zbound ) go to 340
        if ( iub - penalt .le. z ) go to 690
        zbound = penalt
  340   if ( penalt .lt. izmax ) go to 370
        if ( penalt .eq. izmax ) go to 350
        izmax = penalt
        rzmax = ds(jj)
        go to 360
  350   if ( ds(jj) .le. rzmax ) go to 370
        rzmax = ds(jj)
  360   j = jj
        jt = ka(jj)
  370 continue
c
c compute the improved martello-toth bound (iub - zbound).
c
      if ( ubl(l-1) .gt. iub - zbound ) ubl(l-1) = iub - zbound
      if ( numnod .gt. 1 ) go to 390
      jklim = iub - zbound
      if ( jub .gt. ubl(1) ) jub = ubl(1)
      if ( jub .gt. jklim ) jub = jklim
c
c second reduction (executed only for the root node).
c
      call gr2(n,m,p,w,q,b,a,mind,pak,kap,pakl,ip,ir,il,if,nr,z,
     &         xstar,ub(1,1),x,v,flrep,kvst,jdimr,jdimc,jnlev,dmyr1)
      if ( nr .eq. 0 ) go to 880
c
c check for re-execution.
c
      do 380 i=1,m
        if ( flrep(i) .eq. 0 ) go to 380
        numnod = 0
        kub = jub
        go to 30
  380 continue
c
c forward step on item j.
c
  390 e(l) = j
      type(l) = jt
      t = 0
      vc = 0
      if ( type(l) .gt. 0 ) go to 420
      do 410 i=1,m
        if ( w(i,j) .gt. q(i) ) go to 400
        if ( a(i,j) .eq. (- 1) ) go to 400
        t = t + 1
        d(l,t) = i
  400   vc = vc + v(l-1,i)
  410 continue
      if ( t .eq. 0 ) go to 690
      go to 450
  420 do 440 i=1,m
        if ( x(i,j) .eq. 0 ) go to 430
        t = t + 1
        d(l,t) = i
        go to 440
  430   vc = vc + v(l-1,i)
  440 continue
  450 cs(l) = t
c
c step 3 (bound).
c
c compute the initial martello-toth bound for the son nodes.
c
      kk = 0
      do 560 r=1,t
        h = d(l,r)
        qh = q(h)
        if (type(l) .eq. 0 ) qh = qh - w(h,j)
        jw = w(h,j)
        w(h,j) = inf
        u = 0
        sb = 0
        isu = 0
        do 480 js=1,n
          s = mind(h,js)
          if ( b(s) .eq. 0 ) go to 460
          if ( w(h,s) .gt. qh ) go to 470
          if ( a(h,s) .ne. 0 ) go to 470
          u = u + 1
          ps(u) = p(h,s)
          ws(u) = w(h,s)
          sb = sb + ws(u)
          bs(u) = s
          go to 480
  460     if ( a(h,s) .ne. 1 ) go to 470
          call ydef(l,h,s,1)
          isu = isu + p(h,s)
          go to 480
  470     call ydef(l,h,s,0)
  480   continue
        if ( type(l) .gt. 0 ) go to 490
        call ydef(l,h,j,1)
        isu = isu + p(h,j)
  490   index = cc(l-1)
        ihs = d(l-1,index)
        ksu = z - ub(l-1,ihs) + v(l-1,h) - isu
        su = ksu
        if ( sb .gt. qh ) go to 510
        su = 0
        if ( u .eq. 0 ) go to 540
        do s=1,u
          xs(s) = 1
          su = su + ps(s)
        end do
        go to 520
  510   mubf = - 1
        call kpmax(u,ps,ws,qh,su,xs,mubf,
     &             jdimc+1,jdimc,dmyc1,dmyc2,dmyc3,dmyc4,dmyc5)
  520   do s=1,u
          js = bs(s)
          call ydef(l,h,js,xs(s))
        end do
  540   if ( su .gt. ksu ) go to 550
        if ( type(l) .eq. 0 ) go to 550
        kk = kk + 1
        if ( kk .eq. 1 ) go to 550
        w(h,j) = jw
        go to 690
  550   su = su + isu
        vs(l,h) = su
        if ( type(l) .gt. 0 ) vc = vc + su
        w(h,j) = jw
  560 continue
c
c step 4 (sort the son nodes).
c
      us(l) = vc
      jsign = 1
      if ( type(l) .eq. 0 ) jsign = - 1
      do r=1,t
        h = d(l,r)
        jadd = v(l-1,h) - vs(l,h)
        ub(l,h) = vc + jadd*jsign
        dd(r) = h
        ud(h) = ub(l,h)
      end do

      call sorti(t,ud,dd,jdimr)

      do r=1,t
        d(l,r) = dd(r)
      end do
c
c step 5 (first branching from a node).
c
      l1 = l - 1
      cc(l) = 1
      h = d(l,1)
      if ( ub(l,h) .le. z  ) go to 690

      do i=1,m
        lb(l,i) = lb(l1,i)
        v(l,i) = v(l1,i)
      end do

      if ( type(l) .eq. 0 ) go to 620

      do u=2,t
        r = d(l,u)
        lb(l,r) = l
        v(l,r) = vs(l,r)
        do s=1,n
          call yuse(l,r,s,x(r,s))
        end do
      end do

      go to 640
  620 lb(l,h) = l
      v(l,h) = vs(l,h)
      do s=1,n
        call yuse(l,h,s,x(h,s))
      end do
  640 a(h,j) = 1
      b(j) = 0
      q(h) = q(h) - w(h,j)
      l = l + 1
      go to 140
c
c step 6 (update the current optimal solution).
c
  650 l1 = l - 1
      index = cc(l1)
      h = d(l1,index)
      if ( z .ge. ub(l1,h) ) go to 690
      z = ub(l1,h)
      do 670 j=1,n
        do 660 i=1,m
          if ( x(i,j) .ne. 1 ) go to 660
          xstar(j) = i
          go to 670
  660   continue
  670 continue
  680 if ( z .eq. jub ) go to 880
c
c step 7 (backtrack and branching for the subsequent son nodes).
c
  690 l = l - 1
      if ( l .le. 1 ) go to 880
      index = cc(l)
      h = d(l,index)
      j = e(l)
      l1 = l - 1
      if ( cc(l) .eq. cs(l) ) go to 700
      jh = d(l,index+1)
      if ( ubl(l1) .le. z ) go to 700
      if ( ub(l,jh) .gt. z ) go to 780
      go to 710
  700 if ( b(j) .eq. 1 ) go to 720
  710 a(h,j) = 0
      b(j) = 1
      q(h) = q(h) + w(h,j)
  720 t = cs(l)
      if ( type(l) .gt. 0 ) go to 750
      ls = lb(l1,h)
      lb(l,h) = ls
      v(l,h) = v(l1,h)
      do 730 u=1,t
        index = d(l,u)
        a(index,j) = 0
  730 continue
      do s=1,n
        call yuse(ls,h,s,x(h,s))
      end do
      go to 690
  750 do 770 u=1,t
        r = d(l,u)
        if ( r .eq. h ) go to 770
        ls = lb(l1,r)
        lb(l,r) = ls
        v(l,r) = v(l1,r)
        a(r,j) = 0
        do 760 s=1,n
          call yuse(ls,r,s,x(r,s))
  760   continue
  770 continue
      go to 690
  780 a(h,j) = - 1
      q(h) = q(h) + w(h,j)
      if ( type(l) .gt. 0 ) go to 810
      ls = lb(l1,h)
      lb(l,h) = ls
      v(l,h) = v(l1,h)
      do s=1,n
        call yuse(ls,h,s,x(h,s))
      end do
      cc(l) = cc(l) + 1
      index = cc(l)
      h = d(l,index)
      lb(l,h) = l
      v(l,h) = vs(l,h)
      do s=1,n
        call yuse(l,h,s,x(h,s))
      end do
      go to 850
  810 lb(l,h) = l
      v(l,h) = vs(l,h)
      do s=1,n
        call yuse(l,h,s,x(h,s))
      end do
      cc(l) = cc(l) + 1
      index = cc(l)
      h = d(l,index)
      ls = lb(l1,h)
      lb(l,h) = ls
      v(l,h) = v(l1,h)
      do s=1,n
        call yuse(ls,h,s,x(h,s))
      end do
      if ( cc(l) .lt. cs(l) ) go to 850
      if ( us(l) .le. z ) go to 850
      if ( cs(l) .eq. m ) go to 850
      do 840 r=1,m
        if ( a(r,j) .eq. (- 1) ) go to 840
        if ( w(r,j) .le. q(r) ) go to 860
  840 continue
  850 a(h,j) = 1
      q(h) = q(h) - w(h,j)
      go to 870
  860 a(h,j) = 0
      b(j) = 1
  870 l = l + 1
      go to 140
c
c terminate.
c
  880 call termin(jfi,invst,jub,imult,z,kvst,numnod,minmax,
     &            m,n,p,lam,jdimr,jdimc,jb,back)
      return
      end
      subroutine mthg ( n, m, p, w, c, minmax, z, xstar, jck )

c*********************************************************************72
c
cc MTHG heuristically solves the generalized assignment problem.
c
c opt z = p(1,1)*x(1,1) + ... + p(1,n)*x(1,n) +
c                         ...                 +
c         p(m,1)*x(m,1) + ... + p(m,n)*x(m,n)
c
c     (where  opt = min  if  minmax = 1 ,  opt = max  if  minmax = 2 )
c
c subject to:
c
c       w(i,1)*x(i,1) + ... + w(i,n)*x(i,n) .le. c(i)  for i=1,...,m,
c       x(1,j) + ... + x(m,j) = 1                      for j=1,...,n,
c       x(i,j) = 0 or 1                     for i=1,...,m, j=1,...,n.
c
c the program is included in the volume
c   s. martello, p. toth, "knapsack problems: algorithms
c   and computer implementations", john wiley, 1990
c and implements the polynomial-time algorithms described
c in section  7.4 .
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. m .le. jdimr ;
c   2) 2 .le. n .le. jdimc ;
c      ( jdimr  and  jdimc  are defined by the first two executable
c       statements;)
c   3) p(i,j), w(i,j) and c(i) positive integers;
c   4) w(i,j) .le. c(i) for at least one i, for j=1,...,n;
c   5) c(i) .ge. min (w(i,j)) for i=1,...,m.
c
c mthg calls 6 procedures: chmthg, feas, gha, ghbcd, ghx and trin.
c
c the program is completely self-contained and communication to it is
c achieved solely through the parameter list of mthg.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mthg needs
c   6 arrays ( c ,  dmyr1 ,  dmyr2 ,  dmyr3 ,  dmyr4  and  dmyr5 ) of
c              length at least  jdimr ;
c   7 arrays ( xstar ,  best ,  dmyc1 ,  dmyc2 ,  dmyc3 ,  dmyc4  and
c              dmycr1 ) of length at least  jdimc ;
c   3 arrays ( p ,  w  and  a ) of length at least  jdimr x jdimc .
c
c the arrays are currently dimensioned to allow problems for which
c       m .le. 50 ,
c       n .le. 500
c (so, in the calling program, arrays  p  and  w  must be dimensioned
c at  (50,500) ). changing such limits necessitates changing the
c dimension of all the arrays in subroutine mthg, as well as the first
c two executable statements.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c n        = number of items;
c m        = number of knapsacks;
c p(i,j)   = profit of item j if assigned to knapsack i
c            (i=1,...,m; j=1,...,n);
c w(i,j)   = weight of item j if assigned to knapsack i
c            (i=1,...,m; j=1,...,n);
c c(i)     = capacity of knapsack i (i=1,...,m);
c minmax   = 1 if the objective function must be minimized,
c          = 2 if the objective function must be maximized;
c jck      = 1 if check on the input data is desired,
c          = 0 otherwise.
c
c meaning of the output parameters:
c z        = value of the solution found if z .gt. 0 ,
c          = 0 if no feasible solution is found,
c          = error in the input data (when jck=1) if z .lt. 0 : condi-
c            tion  - z  is violated;
c xstar(j) = knapsack where item j is inserted in the solution found.
c
c all the parameters are integer. on return of mthg all the input
c parameters are unchanged, but  p(i,j)  is set to  0  for all pairs
c (i,j)  such that  w(i,j) .gt. c(i) .
c
      integer p(50,500),w(50,500),c(50),xstar(500),z
      integer zm
      integer best(500)
      integer a(50,500)
      integer dmyr1(50),dmyr2(50),dmyr3(50),dmyr4(50),dmyr5(50)
      integer dmyc1(500),dmyc2(500),dmyc3(500),dmyc4(500)
      real    dmycr1(500)
c
c  Definition of the internal parameters.
c
      jdimr = 50
      jdimc = 500
      z = 0
      if ( jck .eq. 1 ) call chmthg(n,m,p,w,c,jdimr,jdimc,z)
      if ( z .lt. 0 ) return
c
c  Initialize.
c
      invst = 0
      imult = - 1
      if ( minmax .eq. 2 ) go to 10
c
c  Transform the minimization problem into a maximization problem.
c
      call trin(p,n,m,invst,lam,jdimr,jdimc)
      imult = 1
c
c  Solve the maximization problem.
c
c  Check for infeasibility.
c
   10 call feas(n,m,p,w,c,xstar,jfi,jdimr,jdimc)
      if ( jfi .eq. 1 ) go to 30
c
c  First heuristic solution.
c
      call gha(p,w,c,n,m,z,xstar,iub,best,kvst,inf,
     &         jdimr,jdimc,dmyr1,dmyr2,dmyc1,dmyc2,dmyc3,dmyc4)
      if ( z .eq. iub ) go to 20
c
c  Second heuristic solution.
c
      call ghbcd(p,w,c,n,m,z,xstar,inf,
     &           jdimr,jdimc,dmyc1,dmyr1,dmyr2,dmyr3,dmyr4,dmyr5,
     &           dmyc2,dmyc3,dmyc4,dmycr1,a)
c
c  Terminate.
c
   20 zm = z
      z = 0
      if ( zm .gt. kvst ) z = invst - zm*imult
   30 if ( minmax .eq. 2 ) return
c
c  Restore the original minimization problem.
c
      do i=1,m
        do j=1,n
          if ( p(i,j) .gt. 0 ) p(i,j) = lam - p(i,j)
        end do
      end do

      return
      end
      subroutine mthm ( n, m, p, w, c, z, x, jdn, jdm, li, jck, cr, 
     &  min, xx, x1, f )

c*********************************************************************72
c
cc MTHM heuristically solves the 0-1 multiple knapsack problem.
c
c maximize  z = p(1)*(y(1,1) + ... + y(m,1)) +
c                              ...           +
c               p(n)*(y(1,n) + ... + y(m,n))
c subject to:
c
c   w(1)*y(i,1) + ... + w(n)*y(i,n) .le. c(i)   for i=1,...,m,
c   y(1,j) + ... + y(m,j) .le. 1                for j=1,...,n,
c   y(i,j) = 0 or 1                  for i=1,...,m, j=1,...,n.
c
c the program is included in the volume
c   s. martello, p. toth, "knapsack problems: algorithms
c   and computer implementations", john wiley, 1990
c and implements the polynomial-time algorithms described
c in section  6.6.2 .
c the program derives from an earlier code presented in
c   s. martello, p. toth, "heuristic algorithms for the
c   multiple knapsack problem", computing, 1981.
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdn - 1  and  1 .le. m .le. jdm - 1 ;
c   2) p(j), w(j) and c(i) positive integers;
c   3) min (c(i)) .ge. min (w(j));
c   4) max (w(j)) .le. max (c(i));
c   5) max (c(i)) .lt. w(1) + ... + w(n);
c   6) p(j)/w(j) .ge. p(j+1)/w(j+1) for j=1,...,n-1;
c   7) c(i) .le. c(i+1) for i=1,...,m-1.
c
c mthm can call 6 subroutines:
c   chmthm to check the input data;
c   mgr1 or mgr2 to find an initial feasible solution;
c   rearr to re-arrange a feasible solution;
c   impr1 and impr2 to improve on a feasible solution.
c the user selects the sequence of calls through input parameters.
c
c the program is completely self-contained and communication to it
c is achieved solely through the parameter list of mthm.
c the only machine-dependent constant is used to define  inf  (first
c executable statement), which must be set to a large positive
c integer value.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mthm needs
c   6  arrays ( p ,  w ,  x ,  min ,  xx  and  x1 ) of length at
c               least  jdn ;
c   2  arrays ( c  and  cr ) of length at least  jdm ;
c   1  array  ( f ) of length at least  jdm x jdm .
c in addition, subroutine mgr2 uses
c   7  arrays of length  5 ;
c   1  array  of length  201 ;
c   1  array  of length  5 * 200 ;
c subroutine mgr2 is called only when  m .le. 5  and  n .le. 200 .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c n    = number of items;
c m    = number of knapsacks;
c p(j) = profit of item  j  (j=1,...,n);
c w(j) = weight of item  j  (j=1,...,n);
c c(i) = capacity of knapsack  i  (i=1,...,m);
c jdn  = dimension of arrays  p ,  w ,  x ,  min ,  xx  and  x1 ;
c jdm  = dimension of arrays  c ,  cr  and  f ;
c li   = 0 to output the initial feasible solution,
c      = 1 to also perform subroutines rearr and impr1,
c      = 2 to also perform subroutines rearr, impr1 and impr2;
c jck  = 1 if check on the input data is desired,
c      = 0 otherwise.
c
c meaning of the output parameters:
c z     = value of the solution found if  z .gt. 0 ,
c       = error in the input data (when jck=1) if  z .lt. 0 :
c         condition -z is violated;
c x(j)  = 0  if item  j  is not in the solution found
c         (i.e. if  y(i,j) = 0  for all  i ),
c       = knapsack where item  j  is inserted, otherwise
c         (i.e. if  y(x(j),j) = 1 ).
c
c arrays cr, min, xx, x1 and f are dummy.
c
c all the parameters are integer. on return of mthm all the
c input parameters are unchanged.
c
cf2py intent(in) n, m, p, w, c, jdn, jdm, li, jck
cf2py intent(hide) cr, min, xx, x1, f
cf2py intent(out) z, x
Cf2py depend(jdn) p, w, x, min, xx, x1
Cf2py depend(jdm) c, cr, f
      integer p(jdn),w(jdn),x(jdn),c(jdm),z
      integer min(jdn),xx(jdn),x1(jdn),cr(jdm),f(jdm,jdm),z1

      inf = 999999999
      z = 0
      if ( jck .eq. 1 ) call chmthm(n,m,p,w,c,jdn,jdm,z)
      if ( z .lt. 0 ) return
      if ( m .le. 5 .and. n .le. 200 ) go to 10
      call mgr1(n,p,w,m,c,z,x,cr,inf,jdn,jdm)
      go to 20

   10 continue

      call mgr2(n,p,w,m,c,z,x,cr,inf,jdn,jdm)

   20 continue

      if ( li .eq. 0 ) go to 60

      z1 = z
      do j=1,n
        x1(j) = x(j)
      end do

      call rearr(n,p,w,m,c,z,x,cr,inf,jdn,jdm)

      call impr1(n,p,w,m,z,x,cr,inf,jdn,jdm,f)

      if ( li .ne. 1 ) then

        call impr2(n,p,w,m,z,x,cr,min,xx,inf,jdn,jdm)

      end if

      if ( z .lt. z1 ) then

        z = z1

        do j=1,n
          x(j) = x1(j)
        end do

      end if

   60 continue

      do j=1,n
        if ( x(j) .gt. m ) x(j) = 0
      end do

      return
      end
      subroutine mtm ( n, m, p, w, c, z, x, back, jck, jub )

c*********************************************************************72
c
cc MTM solves the 0-1 multiple knapsack problem.
c
c  Discussion:
c
c    The 0-1 multiple knapsack problem is:
c
c      maximize  z = p(1)*(y(1,1) + ... + y(m,1)) +
c                                   ...           +
c                    p(n)*(y(1,n) + ... + y(m,n))
c      subject to:
c
c        w(1)*y(i,1) + ... + w(n)*y(i,n) .le. c(i)  for i=1,...,m,
c        y(1,j) + ... + y(m,j) .le. 1               for j=1,...,n,
c        y(i,j) = 0 or 1                 for i=1,...,m, j=1,...,n.
c
c the program is included in the volume
c   s. martello, p. toth, "knapsack problems: algorithms
c   and computer implementations", john wiley, 1990
c and implements the enumerative algorithm described in
c section  6.4.3.
c
c the program derives from an earlier code presented in
c   s. martello, p. toth, "algorithm 632. a program for the 0-1
c   multiple knapsack problem", acm transactions on mathematical
c   software, 1985.
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. maxn and 1 .le. m .le. maxm , where  maxn  and
c      maxm  are defined by the first two executable statements;
c   2) p(j), w(j) and c(i) positive integers;
c   3) min (c(i)) .ge. min (w(j));
c   4) max (w(j)) .le. max (c(i));
c   5) max (c(i)) .lt. w(1) + ... + w(n) ;
c   6) p(j)/w(j) .ge. p(j+1)/w(j+1) for j=1,...,n-1;
c   7) c(i) .le. c(i+1) for i=1,...,m-1.
c
c mtm calls  5  procedures: chmtm, par, pi, sigma and skp2.
c
c the program is completely self-contained and communication to it
c is achieved solely through the parameter list of mtm.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mtm needs
c   5  arrays ( c ,  f ,  pbl ,  q  and  v ) of length at least  m ;
c   8  arrays ( p ,  w ,  x ,  ubb ,  bs ,  xs ,  lx  and  lxi ) of
c               length at least  n ;
c   3  arrays ( b ,  ps  and  ws ) of length at least  n + 1 ;
c   3  arrays ( bb ,  xc  and  xl ) of length at least  m x n ;
c   1  array  ( bl ) of length  at least  m x (n + 1) ;
c   5  arrays ( d ,  min ,  pbar ,  wbar  and  zbar ) of length at
c               least  n  (for internal use in subroutine skp2) .
c
c the arrays are currently dimensioned to allow problems for which
c m .le. 10  and  n .le. 1000 . changing such dimensions also requires
c changing the dimensions of  bs ,  ps ,  ws ,  xs ,  lx  and  lxi
c in subroutine sigma, of  bb ,  bl ,  xl ,  bs ,  ps ,  ws  and  xs
c in subroutine pi, of bb ,  lx  and  lxi  in subroutine par, of  d ,
c min ,  pbar ,  wbar  and  zbar  in subroutine skp2. in addition, the
c values of maxn and maxm must be conveniently defined.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c
c n    = number of items;
c
c m    = number of knapsacks;
c
c p(j) = profit of item  j  (j=1,...,n);
c
c w(j) = weight of item  j  (j=1,...,n);
c
c c(i) = capacity of knapsack  i  (i=1,...,m);
c
c back = -1 if exact solution is required,
c      = maximum number of backtrackings to be performed, if
c        heuristic solution is required;
c jck  = 1 if check on the input data is desired,
c      = 0 otherwise.
c
c meaning of the output parameters:
c
c z    = value of the solution found if  z .gt. 0 ,
c      = error in the input data (when jck=1) if  z .lt. 0 :
c        condition -z is violated;
c x(j) = 0  if item  j  is not in the solution found,
c        (i.e. if  y(i,j) = 0  for all  i ),
c      = knapsack where item  j  is inserted, otherwise
c        (i.e. if  y(x(j),j) = 1 );
c jub  = upper bound on the optimal solution value (to evaluate
c        z when back .gt.0 ).
c
c all the parameters are integer. on return of mtm all the
c input parameters are unchanged except  back , which gives
c the number of backtrackings performed.
c
c meaning of the main internal variables:
c
c i       = knapsack currently considered.
c
c lb      = lower bound on the optimal solution.
c
c ub      = upper bound on the optimal solution.
c
c vb      = value of the current solution.
c
c xc(i,j) = 1  if item  j  is inserted in knapsack  i  in
c              the current solution.
c         = 0  otherwise.
c
c f(i)    = pointer to the last item inserted in knapsack  i
c           ( = -1  if knapsack  i  is empty).
c
c bb(i,j) = pointer to the item inserted in knapsack  i
c           just before item  j ( = -1  if  j  is the first
c           item inserted in knapsack  i ).
c
c q(i)    = current available capacity of knapsack  i .
c
c b(j)    = 1  if item  j  is not inserted in any knapsack.
c         = 0  if item  j  is inserted in a knapsack.
c
c pbl(i)  = number of the items which can be inserted in
c           knapsack  i .
c
c bl(i,s) = pointer to the  s-th  item which can be inserted
c           in knapsack  i .
c
c xl(i,j) = 1  if item  j  was inserted in knapsack  i  in
c              the last execution of subroutine pi.
c         = 0  otherwise.
c
cf2py intent(in) n, m, p, w, c, jck, back
cf2py intent(hide) jub
cf2py intent(out) z, x, back
      integer p(1000),w(1000),c(10),x(1000),back,z
      integer bb(10,1000),bl(10,1001),xc(10,1000),xl(10,1000)
      integer b(1001),ubb(1000)
      integer f(10),pbl(10),q(10),v(10),s,u,ub,vb
      integer bs,ps,ws,xs

      common /sngl/ bs(1000),ps(1001),ws(1001),xs(1000)
      common /pub/  lx(1000),lxi(1000),lr,lri,lubi

      maxn = 1000
      maxm = 10
      z = 0

      if ( jck .eq. 1 ) then
        call chmtm(n,m,p,w,c,maxn,maxm,z)
      end if

      if ( z .lt. 0 ) then
        return
      end if

      if ( m .eq. 1 ) go to 230
c
c step 1 (initialization).
c
      jback = back
      back = 0
      kub = 0
      n1 = n + 1
      b(n1) = 1
      m1 = m - 1

      do j=1,n
        b(j) = 1
        do i=1,m
          xc(i,j) = 0
          bb(i,j) = 0
        end do
      end do

      do i=1,m1
        q(i) = c(i)
        f(i) = -1
      end do

      q(m) = c(m)
      z = 0
      vb = 0
      i = 1
      call sigma(n,m,p,w,c,1,b,kub,ub)
      jub = ub
      do j=1,n
        lxi(j) = lx(j)
      end do
      lri = lr
      lubi = ub
      iflag = 0
c
c step 2 (heuristic).
c
   50 continue

      kub = z - vb
      call pi(n,m,p,w,q,i,b,bb,kub,bl,lb,pbl,v,xl)
      if ( lb + vb .le. z ) go to 120
      z = lb + vb

      do j=1,n
        x(j) = 0
        do s=1,i
          if ( xc(s,j) .ne. 0 ) then
            x(j) = s
            go to 65
          end if
        end do
65      continue
      end do

      ip = pbl(i)

      do j=1,ip
        jj = bl(i,j)
        if ( xl(i,j) .eq. 1 ) x(jj) = i
      end do

      i1 = i + 1
      do ii=i1,m
        ip = pbl(ii)
        do j=1,ip
          jj = bl(ii,j)
          if ( xl(ii,j) .eq. 1 ) x(jj) = ii
        end do
      end do

      if ( jback .eq. 1 ) then
        return
      end if

      if ( ub .eq. lb ) go to 180
c
c step 3 (updating).
c
  120 if ( v(i) .eq. 0 ) go to 160
      iuv = ub + vb
      u = pbl(i)
      ibv = 0
      do 150 s=1,u
        if ( xl(i,s) .eq. 0 ) go to 150
        j = bl(i,s)
        xc(i,j) = 1
        q(i) = q(i) - w(j)
        vb = vb + p(j)
        b(j) = 0
        bb(i,j) = f(i)
        ubb(j) = iuv

        if ( iflag .ne. 1 ) then
          lub = iuv
          lj = j
          li = i
        end if

        f(i) = j
        ibv = ibv + p(j)
        if ( ibv .eq. v(i) ) go to 160
        call par(i,i,ub,iflag,vb,lub,lj,li,f,bb,q,b,n)
        if ( iflag .eq. 1 ) go to 140
        kub = z - vb
        call sigma(n,m,p,w,q,i,b,kub,ub)
        lj = n1
  140   iuv = ub + vb
        if ( iuv .le. z ) go to 180
  150 continue

  160 if ( i .eq. m - 1 ) go to 180
      ip1 = i + 1
      call par(ip1,i,ub,iflag,vb,lub,lj,li,f,bb,q,b,n)
      if ( iflag .eq. 1 ) go to 170
      kub = z - vb
      call sigma(n,m,p,w,q,ip1,b,kub,ub)
      lj = n1
  170 if ( ub + vb .le. z ) go to 180
      i = i + 1
      go to 120
c
c step 4 (backtracking).
c
  180 continue

      if ( i .le. 0 ) then
        back = back - 1
        return
      end if

  190 if ( back .eq. jback ) return
      back = back + 1
      if ( f(i) .ne. (-1) ) go to 210

      do j=1,n
        bb(i,j) = 0
      end do

      i = i - 1
      go to 180

  210 continue

      j = f(i)
      xc(i,j) = 0
      b(j) = 1
      vb = vb - p(j)
      q(i) = q(i) + w(j)
      do s=1,n
        if ( bb(i,s) .eq. j ) bb(i,s) = 0
      end do
      f(i) = bb(i,j)
      if ( ubb(j) .le. z ) go to 180
      ub = ubb(j) - vb
      iflag = 1
      go to 50
c
c  Particular case ( 0-1 single knapsack problem).
c
  230 continue

      k1 = c(1)
      call skp2 ( n, p, w, k1, 0, x, z )
      back = 0
      return
      end
      subroutine mtp ( n, w, c, z, xstar, jdim, back, jck, lb, wr,
     &  xstarr, dum, res, rel, x, r, wa, wb, kfix, fixit, xred, ls,
     &  lsb, xheu )

c*********************************************************************72
c
cc MTP solves the bin packing problem
c
c minimize z = y(1) + ... + y(n)
c
c subject to:
c
c        w(1)*x(i,1) + ... + w(n)*x(i,n) .le. c*y(i)  for i=1,...,n,
c        x(1,j) + ... + x(m,j) = 1                    for j=1,...,n,
c        y(i) = 0 or 1                                for i=1,...,n,
c        x(i,j) = 0 or 1                   for i=1,...,n, j=1,...,n
c
c (i.e., minimize the number of bins of capacity  c  needed to allocate
c  n  items of size  w(1),...,w(n) ).
c
c the program is included in the volume
c   s. martello, p. toth, "knapsack problems: algorithms
c   and computer implementations", john wiley, 1990
c and implements the branch-and-bound algorithm described
c in section 8.5 .
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdim;
c   2) w(j) and c positive integers;
c   3) w(j) .le. c for j=1,..., n;
c   4) w(j) .ge. w(j+1) for j=1,...,n-1.
c
c in the output solution (see below) the  z  lowest indexed bins are
c used.
c
c mtp calls 14 procedures: chmtp, enumer, ffdls, fixred, hbfds,
c                          insert, lcl2, l2, l3, mwfds, restor,
c                          search, sorti2 and update.
c
c the program is completely self-contained and communication to it is
c achieved solely through the parameter list of mtp.
c no machine-dependent constant is used .
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mtp needs 17 arrays ( w ,  xstar ,  wr,  xstarr ,  dum ,  res ,
c                       rel ,  x ,  r ,  wa ,  wb ,  kfix ,  fixit ,
c                       xred ,  ls ,  lsb  and  xheu ) of length
c                       at least  jdim .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c
c n        = number of items;
c
c w(j)     = weight of item j;
c
c c        = capacity of the bins;
c
c jdim     = dimension of the 17 arrays;
c
c back     = - 1 if exact solution is required,
c          = maximum number of backtrackings to be performed,
c            if heuristic solution is required;
c
c jck      = 1 if check on the input data is desired,
c          = 0 otherwise.
c
c meaning of the output parameters:
c
c z        = value of the solution found if z .gt. 0 ,
c          = error in the input data (when jck=1) if z .lt. 0 : condi-
c            tion  - z  is violated;
c
c xstar(j) = bin where item j is inserted in the solution found;
c
c lb       = lower bound on the optimal solution value.
c
c all the arrays except w and xstar are dummy.
c
c all the parameters are integer. on return of mtp all the input
c parameters are unchanged except  back , which gives the number of
c backtrackings performed.
c
      integer w(jdim)
      integer xstar(jdim),c,z,back
      integer wr(jdim),xstarr(jdim),dum(jdim),vstar
      integer res(jdim),rel(jdim),x(jdim),r(jdim),wa(jdim),wb(jdim),
     &        kfix(jdim),fixit(jdim),xred(jdim),ls(jdim),lsb(jdim),
     &        xheu(jdim)

      z = 0
      if ( jck .eq. 1 ) call chmtp(n,w,c,jdim,z)
      if ( z .lt. 0 ) return
      lbstar = 0
      nn = 9*n/10
      if ( w(1) + w(nn) .ge. c ) go to 40
c
c try a quick solution.
c
c heuristic.
c
      call ffdls(n,w,c,z,r,xstar,ls,lsb,jdim)
c
c lower bound l1.
c
      isumr = 0
      do j=1,z
        isumr = isumr + r(j)
      end do
      isum = z*c - isumr
      lbstar = (isum - 1)/c + 1
      if ( lbstar .eq. z ) go to 70
c
c improved lower bound.
c
      iss = 0

      do i=1,n
        if ( w(i) + w(n) .le. c ) go to 30
        iss = iss + w(i)
      end do
      go to 70
   30 iss = isum - iss
      lbstar = i - 1 + (iss - 1)/c + 1
      if ( lbstar .eq. z ) go to 70
c
c lower bound l2.
c
      call l2(n,w,c,lbal,jdim)
      if ( lbal .le. lbstar ) go to 60
      lbstar = lbal
      if ( lbstar .eq. z ) go to 70
      go to 60
c
c regular solution.
c
c lower bound l3 and reduction.
c
   40 isum = 0
      do i=1,n
        isum = isum + w(i)
      end do
   60 z = 0
      call l3(n,w,c,0,m,dum,xstar,nf,lb3,n+1,xstarr,isum,z,
     &        res,rel,jdim)
      if ( lb3 .gt. lbstar ) lbstar = lb3
      if ( nf .gt. 0 ) go to 80
   70 back = 0
      lb = lbstar
      return
   80 if ( nf .eq. n ) go to 100
c
c define the reduced problem.
c
      ii = 0

      do i=1,n
        if ( xstar(i) .le. 0 ) then
          ii = ii + 1
          wr(ii) = w(i)
          xstarr(ii) = xstarr(i) - m
        end if
      end do

      go to 120
  100 do i=1,n
        wr(i) = w(i)
      end do
  120 vstar = z - m
      lb = lbstar - m
c
c branch-and-bound.
c
      call enumer(nf,wr,c,xstarr,vstar,lb,back,
     &            x,r,wa,wb,kfix,fixit,xred,ls,lsb,dum,xheu,
     &            res,rel,jdim)
c
c re-build the solution.
c
      z = vstar + m
      lb = lb + m
      ii = 0

      do i=1,n
        if ( xstar(i) .le. 0 ) then
          ii = ii + 1
          xstar(i) = xstarr(ii) + m
        end if
      end do

      return
      end
      subroutine mts(n,w,c,z,x,m2,pers,jdd,itm,xx,ws,zs,sum,
     &               td1,td2,td3)

c*********************************************************************72
c
cc MTS solves a small subset sum problem.
c
c jvbit  must be dimensioned at  31 .
c arrays  td1 ,  td2  and  td3  are used for the dynamic programming
c lists. their dimension  jdd  can be changed according to the number
c of states wanted for the lists (in this case the value of  jdd ,
c used for adjustable dimensions, must be changed).
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(itm),x(itm),c,z
      integer xx(itm),ws(itm),zs(itm),sum(itm)
      integer td1(jdd,2),td2(jdd,2),td3(jdd,2)
      integer jvbit(31)
      integer v,cf,cs,cd
c
c initialize.
c
      cf = c
      cs = c
c
c dynamic programming.
c
      cd = c
      call dinsm(n,w,cd,m2,jdd,td1,td2,td3,nsds,nsdm,m,jflm,jfls,pers)
      jxpack = 0
      if ( jfls .eq. 2 ) go to 10
      if ( jfls .eq. 3 ) go to 20
      call usedin(c,td1,jdd,nsds,loc1)
      jts = td1(loc1,1)
      go to 30
   10 call usedin(c,td2,jdd,nsds,loc1)
      jts = td2(loc1,1)
      go to 30
   20 call usedin(c,td3,jdd,nsds,loc1)
      jts = td3(loc1,1)
   30 if ( jflm .eq. 2 ) go to 40
      call usedin(c,td1,jdd,nsdm,loc2)
      jtm = td1(loc2,1)
      jtml = td1(nsdm,1)
      go to 50
   40 call usedin(c,td2,jdd,nsdm,loc2)
      jtm = td2(loc2,1)
      jtml = td2(nsdm,1)
   50 nold = n
      nold1 = nold + 1
      n = n - m
      jwnold = w(n+1)
c
c optimal initial solution.
c
      loc = loc1
      jsol = jts
      if ( jsol .ge. jtm ) go to 60
      loc = - loc2
      jsol = jtm
   60 call upstar(0,0,n,xx,jsol,loc,x,z,lstar)
      if ( z .eq. c ) go to 470
      if ( n .eq. 0 ) go to 470
      w(n+1) = c + 1
      lim = cf
      minw = w(n)
      cs = c
      do l=1,n
        ll = l
        if ( w(l) .gt. cs ) go to 80
        cs = cs - w(l)
      end do
      ll = n + 1
   80 ll = ll - 1
      if ( cs .gt. 0 ) go to 90
      call upstar(0,ll,n,xx,c,-1,x,z,lstar)
      go to 470
   90 continue
      jvbit(1) = 1
      if ( m2 .le. 1 ) go to 110
      jbit = 1
      do j=2,m2
        jbit = jbit*2
        jvbit(j) = jbit
      end do
  110 jsum = jtml
      do j=1,n
        jj = n - j + 1
        jsum = jsum + w(jj)
        sum(jj) = jsum
      end do
      do j=1,n
        xx(j) = 0
      end do
      lold = n
      ns = nold - m2
      ii = 1
      go to 180
c
c try to insert the ii-th item into the current solution.
c
  140 if ( w(ii) .le. c ) go to 150
      ii = ii + 1
      go to 140
c
c build a new current solution.
c
  150 if ( (cf - c) + sum(ii) .le. z ) go to 440
      cs = c - ws(ii)
      in = zs(ii)
      ll = n
      if ( in .gt. n ) go to 180
      do 160 l=in,n
        ll = l
        if ( w(l) .gt. cs ) go to 170
        cs = cs - w(l)
  160 continue
      go to 180
c
c update the optimal solution.
c
  170 ll = ll - 1
      if ( cs .ne. 0 ) go to 180
      call upstar(ii-1,ll,n,xx,cf,-1,x,z,lstar)
      if ( z .ne. lim ) go to 440
      go to 470
c
c save the current solution.
c
  180 ws(ii) = c - cs
      zs(ii) = ll + 1
      xx(ii) = 1
      noldii = nold1 - ii
      if ( ii .gt. ns ) jxpack = jxpack + jvbit(noldii)
      nn = ll - 1

      do j=ii,nn
        j1 = j + 1
        ws(j1) = ws(j) - w(j)
        zs(j1) = ll + 1
        xx(j1) = 1
        noldj1 = nold1 - j1
        if ( j1 .gt. ns ) jxpack = jxpack + jvbit(noldj1)
      end do

  200 j1 = ll + 1

      do j=j1,lold
        ws(j) = 0
        zs(j) = j
      end do
  220 lold = ll
      c = cs
      if ( jfls .eq. 2 ) go to 230
      if ( jfls .eq. 3 ) go to 240
      call usedin(c,td1,jdd,nsds,loc)
      jts1 = td1(loc,1)
      go to 250
  230 call usedin(c,td2,jdd,nsds,loc)
      jts1 = td2(loc,1)
      go to 250
  240 call usedin(c,td3,jdd,nsds,loc)
      jts1 = td3(loc,1)
  250 if ( c .le. cd ) go to 280
      if ( jflm .eq. 2 ) go to 260
      call usedin(c,td1,jdd,nsdm,loc1)
      jtm = td1(loc1,1)
      go to 270
  260 call usedin(c,td2,jdd,nsdm,loc1)
      jtm = td2(loc1,1)
  270 if ( jts1 .ge. jtm ) go to 280
      newsol = (cf - c) + jtm
      loc = - loc1
      go to 350
  280 newsol = (cf - c) + jts1
      if ( ll .lt. ns ) go to 350
c
c the next item ( ll + 1 ) is one of the last  m2 .
c
  290 if ( jfls .eq. 2 ) go to 300
      if ( jfls .eq. 3 ) go to 310
      jts1 = td1(loc,1)
      jts2 = td1(loc,2)
      go to 320
  300 jts1 = td2(loc,1)
      jts2 = td2(loc,2)
      go to 320
  310 jts1 = td3(loc,1)
      jts2 = td3(loc,2)
  320 if ( iand ( jxpack, jts2 ) .eq. 0 ) go to 330
      loc = loc - 1
      go to 290
  330 newsol = (cf - c) + jts1
      if ( c .gt. cd ) go to 350
c
c no more forward steps are required.
c
      ii = ll + 1
      if ( newsol .le. z ) go to 340
      call upstar(ll,ll,n,xx,newsol,loc,x,z,lstar)
      if ( z .eq. lim ) go to 470
  340 if ( ll .lt. n ) go to 440
      go to 410
c
c the next item is one of the first  ns  or  c .gt. cd .
c
  350 if ( newsol .le. z ) go to 360
      call upstar(ll,ll,n,xx,newsol,loc,x,z,lstar)
      if ( z .eq. lim ) go to 470
  360 if ( ll .ge. n - 2 ) go to 370
      ii = ll + 2
      if ( c .ge. minw ) go to 140
      go to 440
c
c ll .ge. n - 2 .
c
  370 ii = n
      if ( ll .eq. n - 1 ) go to 440
      if ( ll .eq. n ) go to 410
c
c ll = n - 2 .
c
      if ( c .lt. w(n) ) go to 440
      c = c - w(n)
      xx(n) = 1
      if ( jflm .eq. 2 ) go to 380
      call usedin(c,td1,jdd,nsdm,loc)
      jtm = td1(loc,1)
      go to 390
  380 call usedin(c,td2,jdd,nsdm,loc)
      jtm = td2(loc,1)
  390 v = cf - c + jtm
      if ( z .ge. v ) go to 400
      call upstar(n,n,n,xx,v,-loc,x,z,lstar)
      if ( z .eq. lim ) go to 470
  400 c = c + w(n)
      xx(n) = 0
      go to 440
c
c particular backtracking for  xx(n) = 1 .
c
  410 xx(n) = 0
      noldn = nold1 - n
      if ( n .gt. ns ) jxpack = jxpack - jvbit(noldn)
      c = c + w(n)
      v = cf - c
      if ( jflm .eq. 2 ) go to 420
      call usedin(c,td1,jdd,nsdm,loc)
      jtm = td1(loc,1)
      go to 430
  420 call usedin(c,td2,jdd,nsdm,loc)
      jtm = td2(loc,1)
  430 v = v + jtm
      if ( z .ge. v ) go to 440
      call upstar(n,n,n,xx,v,-loc,x,z,lstar)
      if ( z .eq. lim ) go to 470
c
c backtrack.
c
  440 nn = ii - 1
      if ( nn .eq. 0 ) go to 470
      do 450 jj=1,nn
        kk = ii - jj
        if ( xx(kk) .eq. 1 ) go to 460
  450 continue
      go to 470
  460 c = c + w(kk)
      xx(kk) = 0
      noldkk = nold1 - kk
      if ( kk .gt. ns ) jxpack = jxpack - jvbit(noldkk)
      ii = kk + 1
      go to 140
c
c return.
c
  470 w(n+1) = jwnold
      n = nold
      c = cf
      i = n
      if ( lstar .lt. 0 ) go to 530
      if ( jfls .eq. 2 ) go to 480
      if ( jfls .eq. 3 ) go to 490
      jts = td1(lstar,2)
      go to 500
  480 jts = td2(lstar,2)
      go to 500
  490 jts = td3(lstar,2)
  500 ll = jts
      ii = n - m
  510 if ( ii .eq. 0 ) go to 520
      if ( x(ii) .eq. 1 ) go to 520
      ii = ii - 1
      go to 510
  520 jj = n - ii
      if ( jj .gt. m2 ) jj = m2
      go to 560
  530 mlstar = - lstar
      if ( jflm .eq. 2 ) go to 540
      jtm = td1(mlstar,2)
      go to 550
  540 jtm = td2(mlstar,2)
  550 ll = jtm
      jj = m
  560 do 570 j=1,jj
        l = ll/2
        x(i) = ll - l*2
        ll = l
        i = i - 1
  570 continue
      return
      end
      subroutine mtsl ( n, w, c, z, x, jdn, jdd, itmm, jck, wo, ind,
     &  xx, ws, zs, sum, td1, td2, td3 )

c*********************************************************************72
c
cc MTSL solves the subset-sum problem
c
c maximize  z = w(1)*x(1) + ... + w(n)*x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) .le. c ,
c               x(j) = 0 or 1  for j=1,...,n.
c
c the program implements the mixed algorithm described in
c section  4.2.3 .
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdn - 1 ;
c   2) w(j), c  positive integers;
c   3) max (w(j)) .lt. c ;
c   4) w(1) + ... + w(n) .gt. c .
c
c mtsl calls  8  procedures: chmtsl, dinsm, mts, presp, sorti, tab,
c                            upstar and usedin.
c if not present in the library of the host, the user must supply an
c integer function  iand(i1,i2)  which sets  iand  to the bit-by-bit
c logical and of  i1  and  i2 .
c
c communication to the program is  achieved solely through the
c parameter list of mtsl.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mtsl needs
c   2  arrays ( w and x ) of length  at least  jdn ;
c   6  arrays ( wo ,  ind ,  xx ,  ws ,  zs  and  sum ) of length
c               at least  itmm ;
c   3  arrays ( td1 ,  td2  and  td3 ) of length at least  jdd x 2 .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c
c n    = number of items;
c
c w(j) = weight of item  j  (j=1,...,n);
c
c c    = capacity;
c
c jdn  = dimension of arrays  w  and  x ;  jdn must be at least n + 1.
c
c jdd  = maximum length of the dynamic programming lists (suggested
c        value jdd = 5000);
c
c itmm = (maximum number of items in the core problem) + 1 ;
c        itmm = jdn in order to be sure that the optimal solution is
c        found.  itmm .lt. jdn (suggested value itmm = 91) produces
c        an approximate solution which is almost always optimal (to
c        check optimality, see whether z = c );
c
c jck  = 1 if check on the input data is desired,
c      = 0 otherwise.
c
c meaning of the output parameters:
c
c z    = value of the solution found if  z .gt. 0 ,
c      = error in the input data (when jck=1) if z .lt. 0 : condi-
c        tion  - z  is violated;
c
c x(j) = 1 if item  j  is in the solution found,
c      = 0 otherwise.
c
c  Local parameters:
c
c    it   = length of the initial core problem (suggested value it = 30);
c
c    id   = increment of the length of the core problem (suggested value
c           id = 30);
c
c    m2   = number of items to be used for the second dynamic programming
c           list; it must be 2 .le. m2 .le. min(31,n-4) (suggested value
c           m2 = min ( 2.5*alog10(max(w(j))) , 0.8*n ) ). m1 , the number
c           of items to be used for the first dynamic programming list, is
c           automatically determined by the program;
c
c    pers = value used to determine  c bar  according to the formula given
c           in section 4.2.2 (suggested value pers = 1.3).
c
c arrays wo, ind, xx, ws, zs, sum, td1, td2 and td3 are dummy.
c
c all the parameters are integer. on return of mtsl all the input
c parameters are unchanged.
c
      integer w(jdn),x(jdn),c,z
      integer wo(itmm),ind(itmm),xx(itmm),ws(itmm),zs(itmm),sum(itmm)
      integer td1(jdd,2),td2(jdd,2),td3(jdd,2)
      z = 0
      if ( jck .eq. 1 ) call chmtsl(n,w,c,z,jdn)
      if ( z .lt. 0 ) return
      if ( n .gt. 2 ) go to 20
c
c case n = 2 .
c
      if ( w(1) .lt. w(2) ) go to 10
      z = w(1)
      x(1) = 1
      x(2) = 0
      return
   10 z = w(2)
      x(1) = 0
      x(2) = 1
      return
c
c definition of parameters it and id (for the core problem).
c
   20 it = 30
      id = 30
      itm = itmm
      if ( itm .gt. n ) itm = n + 1
      if ( it .gt. itm - 1 ) it = itm - 1
      itc = it
   30 itco = itc
c
c definition and sorting of the core problem.
c
      call presp(n,w,c,itc,itco,ind,nc,ni,jsum)
      call sorti(itc,w,ind,jdn)
      do i=1,itc
        indi = ind(i)
        wo(i) = w(indi)
      end do
      rmaxw = wo(1)
c
c definition of parameters m2 and pers (for dynamic programming).
c
      m2 = 2.5*alog10(rmaxw)
      if ( float(m2) .gt. 0.8*float(itc) ) m2 = 0.8*float(itc)
      if ( m2 .gt. itc - 4 ) m2 = itc - 4
      if ( m2 .lt. 2 ) m2 = 2
      pers = 1.3
      if ( jsum .eq. 0 ) go to 60
c
c all the  wo  are in the solution of the core problem.
c
      do i = 1, itc
        x(i) = 1
      end do
      nz = jsum
      go to 90
   60 if ( itc .gt. m2 ) go to 80
      nz = 0
      do i=1,itc
        x(i) = 0
      end do
      go to 90
   80 call mts(itc,wo,nc,nz,x,m2,pers,jdd,itm,xx,ws,zs,sum,
     &         td1,td2,td3)
   90 if ( nz .eq. nc ) go to 100
      if ( itco .eq. itm - 1 ) go to 100
c
c repeat.
c
      itc = itco + id
      if ( itc .gt. itm - 1 ) itc = itm - 1
      go to 30
c
c final solution (optimal if  z = c  or  itm - 1 .eq. n ).
c
  100 z = nz + c - nc

      do i=1,itc
        wo(i) = x(i)
      end do

      do i=1,ni
        x(i) = 1
      end do

      nl = ni + 1
      do i=nl,n
        x(i) = 0
      end do

      do i=1,itc
        jj = ind(i)
        x(jj) = wo(i)
      end do

      return
      end
      subroutine mtu1(n,p,w,c,rn,z,x,jdim,jub,xx,min)

c*********************************************************************72
c
cc MTU1 solves the unbounded single knapsack problem
c
c maximize  z = p(1)*x(1) + ... + p(n)*x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) .le. c ,
c               x(j) .ge. 0 and integer  for j=1,...,n.
c
c the program is based on the branch-and-bound algorithm presented in
c  s. martello, p. toth, "branch and bound algorithms for the solution
c  of the general unidimensional knapsack problem", in m. roubens, ed.,
c  "advances in operations research", north holland, 1977.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
cf2py intent(in) n, p, w, c, rn, jdim
cf2py intent(hide) xx, min
cf2py intent(out) z, x, jub
cf2py depend(jdim) p, w, xx, x, min
      integer p(jdim),w(jdim),c,z
      integer xx(jdim),cws,cwf,diff,r,s,s1,s2,t,profit,ps
      real    x(jdim),min(jdim)
c
c step 1.
c
      cwf = c
      s1 = c/w(1)
      s2 = (c - s1*w(1))/w(2)
      ip = s1*p(1) + s2*p(2)
      cws = c - s1*w(1) - s2*w(2)

      if ( cws .eq. 0 ) then

        z = ip
        jub = z
        x(1) = s1
        x(2) = s2
        do j=3,n
          x(j) = 0
        end do
        return

      end if

      mink = c + 1
      min(n) = mink

      do j=2,n
        k = n + 2 - j
        if ( w(k) .lt. mink ) mink = w(k)
        min(k-1) = mink
      end do
      w(n+1) = c + 1
      p(n+1) = 0
      lim = ip + cws*p(3)/w(3)
      if ( n .eq. 2 ) lim = float(ip) + float(cws)*rn
      itrunc = (w(2) - cws + w(1) - 1)/w(1)
      lim12 = ip + (cws + itrunc*w(1))*p(2)/w(2) - itrunc*p(1)
      if ( lim12 .gt. lim ) lim = lim12
      jub = lim
      z = 0
      xx(1) = s1
      xx(2) = s2

      do j=3,n
        xx(j) = 0
      end do

      profit = ip
      c = cws
      ii = 2
      go to 110
c
c step 2.
c
   60 s = c/w(ii)
      if ( s .gt. 0 ) go to 70
      if ( z .ge. profit + c*p(ii+1)/w(ii+1) ) go to 120
      ii = ii + 1
      go to 60
c
c step 3.
c
   70 ps = profit + s*p(ii)
      cws = c - s*w(ii)
      if ( ( cws .eq. 0 ) .or. ( ii .eq. n ) ) go to 80
      if ( z .ge. ps + cws*p(ii+1)/w(ii+1) ) go to 150
      c = cws
      profit = ps
      xx(ii) = s
      go to 110
   80 if ( z .ge. ps ) go to 150
      z = ps
      ii1 = ii - 1
      do j=1,ii1
        x(j) = xx(j)
      end do
      x(ii) = s
      ii1 = ii + 1
      do j=ii1,n
        x(j) = 0
      end do
      if ( z .ne. lim ) go to 150
      c = cwf
      return
c
c step 4.
c
  110 ii = ii + 1
      if ( c .ge. int(min(ii-1)) ) go to 60
c
c step 5.
c
  120 if ( z .ge. profit ) go to 140
      z = profit
      do j=1,n
        x(j) = xx(j)
      end do
      if ( z .ne. lim ) go to 140
      c = cwf
      return
  140 if ( xx(n) .eq. 0 ) go to 150
      c = c + xx(n)*w(n)
      profit = profit - xx(n)*p(n)
      xx(n) = 0
c
c step 6.
c
  150 ib = ii - 1
      do j=1,ib
        kk = ii - j
        if ( xx(kk) .gt. 0 ) go to 170
      end do
      c = cwf
      return
  170 r = c
      c = c + w(kk)
      profit = profit - p(kk)
      xx(kk) = xx(kk) - 1
      if ( z .lt. profit + c*p(kk + 1)/w(kk + 1) ) go to 180
      c = c + xx(kk)*w(kk)
      profit = profit - xx(kk)*p(kk)
      xx(kk) = 0
      ii = kk + 1
      go to 150
  180 if ( r .lt. int(min(kk)) ) go to 190
      ii = kk + 1
      go to 60
  190 nn = kk + 1
      ii = kk + 1
c
c step 7.
c
  200 diff = w(nn) - w(kk)
      if ( diff ) 210,260,220
  210 t = r - diff
      if ( t .lt. int(min(nn-1)) ) go to 260
      s = c/w(nn)
      ii = nn
      go to 70
  220 if ( diff .gt. r ) go to 260
      if ( z .ge. profit + p(nn) ) go to 260
      z = profit + p(nn)
      do 230 j=1,kk
        x(j) = xx(j)
  230 continue
      kk1 = kk + 1
      do j=kk1,n
        x(j) = 0
      end do
      x(nn) = 1
      if ( z .ne. lim ) go to 250
      c = cwf
      return
  250 r = r - diff
      kk = nn
c
c step 8.
c
  260 if ( z .ge. profit + c*p(nn+1)/w(nn+1) ) go to 150
      nn = nn + 1
      go to 200
      end
      subroutine mtu2(n,p,w,c,z,x,jdim,jfo,jck,jub,po,wo,xo,rr,pp)

c*********************************************************************72
c
cc MTU2 solves the unbounded single knapsack problem
c
c maximize  z = p(1)*x(1) + ... + p(n)*x(n)
c
c subject to:   w(1)*x(1) + ... + w(n)*x(n) .le. c ,
c               x(j) .ge. 0 and integer  for j=1,...,n.
c
c the program is included in the volume
c   s. martello, p. toth, "knapsack problems: algorithms
c   and computer implementations", john wiley, 1990
c and implements the enumerative algorithm described in
c section  3.6.3 .
c
c the input problem must satisfy the conditions
c
c   1) 2 .le. n .le. jdim - 1 ;
c   2) p(j), w(j), c  positive integers;
c   3) max (w(j)) .le. c .
c
c mtu2   calls  5  procedures: chmtu2, ksmall, mtu1, redu and sortr.
c ksmall calls  8  procedures: bld, bldf, blds1, detns1, detns2,
c                              forwd, mpsort and sort7.
c
c the program is completely self-contained and communication to it is
c achieved solely through the parameter list of mtu2.
c no machine-dependent constant is used.
c the program is written in 1967 american national standard fortran
c and is accepted by the pfort verifier (pfort is the portable
c subset of ansi defined by the association for computing machinery).
c the program has been tested on a digital vax 11/780 and an h.p.
c 9000/840.
c
c mtu2 needs  8  arrays ( p ,  w ,  x ,  po ,  wo ,  xo ,  rr  and
c                        pp ) of length at least  jdim .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
c meaning of the input parameters:
c n    = number of item types;
c p(j) = profit of each item of type  j  (j=1,...,n);
c w(j) = weight of each item of type  j  (j=1,...,n);
c c    = capacity of the knapsack;
c jdim = dimension of the 8 arrays;
c jfo  = 1 if optimal solution is required,
c      = 0 if approximate solution is required;
c jck  = 1 if check on the input data is desired,
c      = 0 otherwise.
c
c meaning of the output parameters:
c z    = value of the solution found if  z .gt. 0 ,
c      = error in the input data (when jck=1) if z .lt. 0 : condi-
c        tion  - z  is violated;
c x(j) = number of items of type  j  in the solution found;
c jub  = upper bound on the optimal solution value (to evaluate z
c        when jfo=0).
c
c arrays po, wo, xo, rr and pp are dummy.
c
c all the parameters but xo and rr are integer. on return of mtu2
c all the input parameters are unchanged.
cf2py intent(in) n, p, w, c, jdim, jfo, jck
cf2py intent(hide) po, wo, xo, rr, pp
cf2py intent(out) z, x, jub
cf2py depend(jdim) p, w, x, po, wo, pp, rr, xo
      integer p(jdim),w(jdim),x(jdim),po(jdim),wo(jdim),pp(jdim),c,z
      real    rr(jdim),xo(jdim)
      z = 0
      if ( jck .eq. 1 ) call chmtu2(n,p,w,c,z,jdim)
      if ( z .lt. 0 ) return
c
c heuristic solution through the core problem.
c
      kc = n
      if ( n .le. 200 ) go to 180
      do j=1,n
        rr(j) = float(p(j))/float(w(j))
      end do
      kc = n/100
      if ( kc .lt. 100 ) kc = 100
      if ( jfo .eq. 0 ) kc = 100
      kk = n - kc + 1
      call ksmall(n,rr,kk,(n+5)/6,xo)
      rk = rr(kk)
      if = 0
      il = n + 1
      do 30 j=1,n
        rr(j) = float(p(j))/float(w(j))
        if ( rr(j) .lt. rk ) go to 30
        if ( rr(j) .eq. rk ) go to 20
        if = if + 1
        pp(if) = j
        go to 30
   20   il = il - 1
        pp(il) = j
   30 continue

      if ( if .eq. 0 ) go to 50
      call sortr(if,rr,pp,jdim)

      do j=1,if
        i = pp(j)
        po(j) = p(i)
        wo(j) = w(i)
        w(i) = - w(i)
      end do

   50 if1 = if + 1

      do j=if1,kc
        i = pp(il)
        po(j) = p(i)
        wo(j) = w(i)
        w(i) = - w(i)
        pp(j) = i
        il = il + 1
      end do

      jpk = po(kc)
      jwk = wo(kc)
c
c reduction of the core problem.
c
      call redu(kc,po,wo,jdim,jpx,x)
      k = 0
      j = jpx
 70   k = k + 1
      po(k) = po(j)
      wo(k) = wo(j)
      pp(k) = pp(j)
      j = x(j)
      if ( j .gt. 0 ) go to 70
      if ( k .gt. 1 ) go to 80
      xo(1) = c/wo(1)
      ixo1 = xo(1)
      z = po(1)*ixo1
      jub = z + (c - wo(1)*ixo1)*jpk/jwk
      po(2) = jpk
      wo(2) = jwk
      go to 90
c
c solution of the reduced core problem.
c
   80 call mtu1(k,po,wo,c,rk,z,xo,jdim,jub,x,rr)
   90 if ( jfo .eq. 0 .or. z .eq. jub ) go to 140
      ip1 = po(1)
      ip2 = po(2)
      iw1 = wo(1)
      iw2 = wo(2)
      ip3 = po(3)
      iw3 = wo(3)
      if ( k .gt. 2 ) go to 100
      ip3 = ip2
      iw3 = iw2
  100 do 130 j=1,n
        x(j) = 0
        if ( w(j) .gt. 0 ) go to 110
        w(j) = - w(j)
        go to 130
  110   icr = c - w(j)
        is1 = icr/iw1
        ib = p(j) + is1*ip1 + (icr - is1*iw1)*ip2/iw2
        if ( ib .le. z ) go to 130
        icrr = icr - is1*iw1
        is2 = icrr/iw2
        ip = p(j) + is1*ip1 + is2*ip2
        icws = icrr - is2*iw2
        ib = ip + icws*ip3/iw3
        itrunc = (iw2 - icws + iw1 - 1)/iw1
        lim12 = ip + (icws + itrunc*iw1)*ip2/iw2 - itrunc*ip1
        if ( lim12 .gt. ib ) ib = lim12
        if ( ib .le. z ) go to 130
        do jj=j,n
          w(jj) = iabs(w(jj))
        end do
        go to 180
  130 continue
      go to 160
  140 do 150 j=1,n
        x(j) = 0
        w(j) = iabs(w(j))
  150 continue
  160 do j=1,k
        i = pp(j)
        x(i) = xo(j)
      end do
      jub = z
      return
c
c solution through complete sorting.
c
  180 do j=1,kc
        rr(j) = float(p(j))/float(w(j))
      end do
      do j=1,n
        pp(j) = j
      end do
      call sortr(n,rr,pp,jdim)
      do j=1,n
        i = pp(j)
        po(j) = p(i)
        wo(j) = w(i)
      end do
c
c reduction of the problem.
c
      call redu(n,po,wo,jdim,jpx,x)
      kf = 0
      j = jpx
  220   kf = kf + 1
        po(kf) = po(j)
        wo(kf) = wo(j)
        pp(kf) = pp(j)
        j = x(j)
      if ( j .gt. 0 ) go to 220
      if ( kf .gt. 1 ) go to 230
      xo(1) = c/wo(1)
      ixo1 = xo(1)
      z = po(1)*ixo1
      jub = z
      go to 240
  230 call mtu1(kf,po,wo,c,0.,z,xo,jdim,jub,x,rr)
  240 do j=1,n
        x(j) = 0
      end do

      do j=1,kf
        i = pp(j)
        x(i) = xo(j)
      end do

      return
      end
      subroutine mwfds(n,w,c,m,kk,k,llb,x,jdim)

c*********************************************************************72
c
cc MWFDS performs a modified worst-fit decreasing heuristic.
c
c  Discussion:
c
c    For local use with current solution given.
c
c    Time complexity  o(n**2) .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim),c,k(jdim),x(jdim),kk(jdim)
      do j=1,m
        k(j) = kk(j)
      end do
      i1 = 1
      if ( m .ge. llb ) go to 30
      m1 = m + 1
      i = 0
      do j=m1,llb
        i = i + 1
        k(j) = c - w(i)
        x(i) = j
      end do
      m = llb
      i1 = i + 1
      if ( i1 .gt. n ) return
   30 do 60 i=i1,n
c
c insert the next item.
c
        iwi = w(i)
        maxres = - 1
        do 40 j=1,m
          kres = k(j) - iwi
          if ( kres .lt. 0 ) go to 40
          if ( kres .le. maxres ) go to 40
          maxres = kres
          jm = j
   40   continue
        if ( maxres .ge. 0 ) go to 50
c
c initialize a new bin.
c
        m = m + 1
        k(m) = c - iwi
        x(i) = m
        go to 60
c
c insert the item into an old bin.
c
   50   k(jm) = k(jm) - iwi
        x(i) = jm
   60 continue

      return
      end
      subroutine newb(c,val,minw0,ipn,iwn,fp1,fpn1,fw1,iubf0)

c*********************************************************************72
c
cc NEWB improves on the current upper bound.
c
c  Discussion:
c
c    The current bound is IUBF0.  This routine improves the bound by
c    taking into account the items following the core problem.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer c,val

      if ( c .ge. minw0 ) then

        ib = val + c*ipn/iwn

      else

        a = minw0 - c
        ib = float(val) + fpn1 - a*fp1/fw1

      end if

      iubf0 = max ( iubf0, ib )

      return
      end
      subroutine par(i,ii,ub,iflag,vb,lub,lj,li,f,bb,q,b,n)

c*********************************************************************72
c
cc PAR does a parametric computation of the upper bounds.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer      f(10),bb(10,1000),q(10),b(1001),ub,vb,r,s
      common /pub/ lx(1000),lxi(1000),lr,lri,lubi
      iflag = 0
      if ( b(lj) .ne. 0 ) go to 60
      i1 = i - 1
      if ( i1 .lt. li ) go to 20
      iq = 0
      do r=li,i1
        iq = iq + q(r)
      end do
      if ( iq .gt. lr ) return
   20 r = ii
      s = f(r)
   30 if ( s .ne. (-1) ) go to 40
      r = r - 1
      s = f(r)
      go to 30
   40 if ( lx(s) .eq. 0 ) return
      if ( s .eq. lj ) go to 50
      s = bb(r,s)
      go to 30
   50 ub = lub - vb
      iflag = 1
      return
   60 i1 = i - 1
      if ( i1 .lt. 1 ) go to 80
      iq = 0
      do 70 r=1,i1
        iq = iq + q(r)
   70 continue
      if ( iq .gt. lri ) return
   80 do 90 j=1,n
        if ( b(j) .eq. 1 ) go to 90
        if ( lxi(j) .eq. 0 ) return
   90 continue
      ub = lubi - vb
      iflag = 1
      return
      end
      subroutine pen0(j,m,p,w,q,a,v,l1,pak,kap,pakl,ip,ir,il,if,
     &                penalt,jfo,jub,jz,inf,jdimr,jdimc,jnlev)

c*********************************************************************72
c
cc PEN0 computes the penalty for an item which was assigned to no knapsack.
c
c jfo = 1 iff item  j  can be inserted in only one knapsack.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),q(jdimr),a(jdimr,jdimc),
     &        v(jnlev,jdimr),pak(jdimr,jdimc),kap(jdimr,jdimc),
     &        pakl(jdimr),ip(jdimr),ir(jdimr),il(jdimr),if(jdimr)
      integer penalt,pmin
      pmin = inf
      jfo = 0
      do 80 i=1,m
        if ( a(i,j) .eq. (- 1) ) go to 80
        if ( w(i,j) .gt. q(i) ) go to 80
        jfo = jfo + 1
        jj = kap(i,j)
        kl = il(i)
        if ( jj .le. kl ) go to 70
        kr = ir(i) - w(i,j)
        kp = ip(i) + p(i,j)
   10   if ( kr .ge. 0 ) go to 20
        jkl = pak(i,kl)
        kr = kr + w(i,jkl)
        kp = kp - p(i,jkl)
        kl = kl - 1
        go to 10
   20   krtot = q(i) - w(i,j)
        w(i,j) = w(i,j) + inf
   30   kl = kl + 1
        if ( kl .le. pakl(i) ) go to 40
        rub = kp
        go to 60
   40   jkl = pak(i,kl)
        if ( w(i,jkl) .gt. kr ) go to 50
        kr = kr - w(i,jkl)
        kp = kp + p(i,jkl)
        go to 30
   50   if ( w(i,jkl) .gt. krtot ) go to 30
        rub = float(kp) + float(p(i,jkl)*kr)/float(w(i,jkl))
   60   iub = rub
        w(i,j) = w(i,j) - inf
        lam = v(l1,i) - ( iub + if(i) )
        if ( lam .le. 0 ) go to 70
        if ( lam .lt. pmin ) pmin = lam
        if ( jub - lam .le. jz ) jfo = jfo - 1
        go to 80
   70   pmin = 0
   80 continue
      penalt = pmin
      return
      end
      subroutine pen1(j,m,p,w,x,v,l1,pak,kap,pakl,ip,ir,il,if,penalt,
     &                jfo,jub,jz,jdimr,jdimc,jnlev)

c*********************************************************************72
c
cc PEN1 computes the penalty for an item assigned to more than one knapsack.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),x(jdimr,jdimc),
     &        v(jnlev,jdimr),pak(jdimr,jdimc),kap(jdimr,jdimc),
     &        pakl(jdimr),ip(jdimr),ir(jdimr),il(jdimr),if(jdimr)
      integer penalt,psum,pmax
      psum = 0
      pmax = 0
      jfo = 0
      do 80 i=1,m
        if ( x(i,j) .eq. 0 ) go to 80
        jj = kap(i,j)
        kl = il(i)
        kp = ip(i)
        kr = ir(i)
        if ( jj - (kl + 1) ) 30,10,80
   10   if ( kl + 1 .eq. pakl(i) ) go to 20
        jkl = pak(i,kl+2)
        rub = float(kp) + float(p(i,jkl)*kr)/float(w(i,jkl))
        go to 70
   20   rub = kp
        go to 70
   30   kp = kp - p(i,j)
        kr = kr + w(i,j)
        la = kl + 1
        l2 = pakl(i)
        if ( la .le. l2 ) go to 40
        rub = kp
        go to 70
   40   do 50 jl=la,l2
          jj = pak(i,jl)
          if ( w(i,jj) .gt. kr ) go to 60
          kr = kr - w(i,jj)
          kp = kp + p(i,jj)
   50   continue
        rub = kp
        go to 70
   60   rub = float(kp) + float(p(i,jj)*kr)/float(w(i,jj))
   70   iub = rub
        lam = v(l1,i) - (iub + if(i))
        if ( lam .le. 0 ) go to 80
        psum = psum + lam
        if ( lam .gt. pmax ) pmax = lam
        if ( jub - lam .le. jz ) jfo = i
   80 continue
      penalt = psum - pmax
      return
      end
      subroutine pi ( n, m, p, w, q, i, b, bb, kub, bl, lb, pbl, v, xl )

c*********************************************************************72
c
cc PI computes a feasible solution to the current problem.
c
c  Discussion:
c
c    The solution is stored in array XL, the corresponding value in LB.
c
c  Modified:
c
c    23 October 2013
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer bb(10,1000),bl(10,1001),xl(10,1000)
      integer p(1000),w(1000),q(10),b(1001),pbl(10),v(10)
      integer bs,pb,ps,qs,sb,u,ws,xs

      common /sngl/ bs(1000),ps(1001),ws(1001),xs(1000)
c
c step 1.
c
      u = 0
      do j=1,n
        if ( b(j) .ne. 0 ) then
          u = u + 1
          bs(u) = j
        end if
      end do

      do j=i,m
        pbl(j) = 0
        v(j) = 0
      end do

      lb = 0
      ikub = kub
      if ( u .eq. 0 ) then
        return
      end if

      ns = 0
      sb = 0
      do j=1,u
        jj = bs(j)
        if ( bb(i,jj) .eq. 0 ) then
          if ( w(jj) .le. q(i) ) then
            ns = ns + 1
            sb = sb + w(jj)
            bl(i,ns) = jj
            ps(ns) = p(jj)
            ws(ns) = w(jj)
          end if
        end if
      end do

      ii = i
c
c step 2.
c
   40 continue

      pbl(ii) = ns
      if ( sb .gt. q(ii) ) go to 60
      pb = 0
      if ( ns .eq. 0 ) go to 80
      do j=1,ns
        pb = pb + ps(j)
        xl(ii,j) = 1
      end do
      go to 80
   60 qs = q(ii)
      kub = 0
      if ( ii .eq. m ) kub = ikub
      call skp2(ns,ps,ws,qs,kub,xs,pb)
      do j=1,ns
        xl(ii,j) = xs(j)
      end do
   80 continue

      lb = lb + pb
      ikub = ikub - pb
      v(ii) = pb
      bl(ii,ns+1) = n + 1
c
c step 3.
c
      if ( ii .eq. m ) return
      jb = 1
      jbs = 0
      do 100 j=1,u
        if ( bs(j) .lt. bl(ii,jb) ) go to 90
        jb = jb + 1
        if ( xl(ii,jb-1) .eq. 1 ) go to 100
   90   jbs = jbs + 1
        bs(jbs) = bs(j)
  100 continue
      u = jbs
      if ( u .eq. 0 ) return
      ns = 0
      sb = 0
      ii = ii + 1
      do 110 j=1,u
        jj = bs(j)
        if( w(jj) .gt. q(ii) ) go to 110
        ns = ns + 1
        sb = sb + w(jj)
        bl(ii,ns) = jj
        ps(ns) = p(jj)
        ws(ns) =  w(jj)
  110 continue

      go to 40
      end
      subroutine prepen(n,m,p,w,q,b,a,mind,pak,kap,pakl,ip,ir,il,if,
     &                  jdimr,jdimc)

c*********************************************************************72
c
cc PREPEN determines  pak ,  kap  and  pakl (pointers for computing penalties)
c and  ip ,  ir ,  il  and  if (greedy initial solutions).
c if = profit of fixed items;
c il = break item (last which fits);
c ip = profit of the first  il  items;
c ir = residual capacity corresponding to  ip .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),q(jdimr),b(jdimc),
     &        a(jdimr,jdimc),mind(jdimr,jdimc),pak(jdimr,jdimc),
     &        kap(jdimr,jdimc),pakl(jdimr),ip(jdimr),ir(jdimr),
     &        il(jdimr),if(jdimr)

      do 60 i=1,m
        k = 0
        isfix = 0
        do 20 js=1,n
          j = mind(i,js)
          if ( b(j) .eq. 0 ) go to 10
          if ( w(i,j) .gt. q(i) ) go to 20
          if ( a(i,j) .eq. (- 1) ) go to 20
          k = k + 1
          pak(i,k) = j
          kap(i,j) = k
          go to 20
   10     if ( a(i,j) .eq. 1 ) isfix = isfix + p(i,j)
   20   continue
        pakl(i) = k
        lk = 0
        ipk = 0
        irk = q(i)
        if ( k .eq. 0 ) go to 50
        do 30 jk=1,k
          j = pak(i,jk)
          if ( w(i,j) .gt. irk ) go to 40
          irk = irk - w(i,j)
          ipk = ipk + p(i,j)
   30   continue
        lk = k
        go to 50
   40   lk = jk - 1
   50   ip(i) = ipk
        ir(i) = irk
        il(i) = lk
        if(i) = isfix
   60 continue
      return
      end
      subroutine presp(n,w,c,itc,itco,ind,nc,ni,jsum)

c*********************************************************************72
c
cc PRESP defines the core problem.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(n),ind(itco)
      integer c
      nc = c
      do i=1,n
        if ( w(i) .gt. nc ) go to 20
        nc = nc - w(i)
      end do
   20 ll = i - 1
      inch = itc/2
      if ( itc - inch .gt. n - ll ) inch = itc - (n - ll)
      if ( inch .gt. ll ) inch = ll
      l = ll
      jsum = 0
      lli = ll

      do i=1,inch
        nc = nc + w(lli)
        ind(i) = lli
        jsum = jsum + w(lli)
        l = l - 1
        if ( i .lt. inch ) lli = lli - (lli - 1)/(inch - i)
      end do

      ni = ll
      iav = itc - inch
      if ( ll + iav .gt. n ) iav = n - ll
      l = inch + 1
      lli = ll + 1

      do i=1,iav
        if ( w(lli) .le. nc ) then
          ind(l) = lli
          jsum = jsum + w(lli)
          l = l + 1
          if ( i .lt. iav ) lli = lli + (n - lli)/(iav - i)
        end if
      end do

      itc = l - 1
      if ( jsum .gt. nc ) jsum = 0
      return
      end
      subroutine rearr(n,p,w,m,c,z,x,cr,inf,jdn,jdm)

c*********************************************************************72
c
cc REARR re-arranges the initial solution.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdn),w(jdn),c(jdm),x(jdn),cr(jdm),z
      z = 0
      i = 1
      j = n
      ibar = 1
      do kk=1,m
        cr(kk) = c(kk)
      end do
      mp1 = m + 1
      p(n+1) = 0
      w(n+1) = inf
   20 if ( x(j) .eq. mp1 ) go to 40
      if ( w(j) .gt. cr(i) ) go to 30
      x(j) = i
      cr(i) = cr(i) - w(j)
      z = z + p(j)
      go to 40
   30 i = i + 1
      if ( i .gt. m ) i = 1
      if ( i .ne. ibar ) go to 20
      x(j) = mp1
      i = i - 1
   40 j = j - 1
      if ( j .eq. 0 ) go to 50
      i = i + 1
      if ( i .gt. m ) i = 1
      ibar = i
      go to 20
   50 maxc = cr(1)
      imaxc = 1
      do 60 i=2,m
        if ( cr(i) .le. maxc ) go to 60
        maxc = cr(i)
        imaxc = i
   60 continue
      do 80 j=1,n
        if ( x(j) .lt. mp1 ) go to 80
        if ( w(j) .gt. maxc ) go to 80
        cr(imaxc) = cr(imaxc) - w(j)
        z = z + p(j)
        x(j) = imaxc
        maxc = cr(1)
        imaxc = 1
        do 70 i=2,m
          if ( cr(i) .le. maxc ) go to 70
          maxc = cr(i)
          imaxc = i
   70   continue
   80 continue
      return
      end
      subroutine redns(n,p,w,izc,iz1,icw,ff,nnfo,nnf,nf,fn1,fn0)

c*********************************************************************72
c
cc REDNS reduces, without sorting, the items not in core.
c
c  Discussion:
c
c    ff(1) to ff(nnfo) = free items (input);
c    ff(1) to ff(nnf) = free items (output).
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(n),w(n),ff(n),nf(n),fn1,fn0
c
c determine the break item jbr.
c
      ic = icw
      ip = 0
      do i=1,nnfo
        j = ff(i)
        if ( w(j) .gt. ic ) go to 20
        ic = ic - w(j)
        ip = ip + p(j)
      end do
   20 jbr = i
c
c reduce the items in n1.
c
      ir = fn1
   30 if ( ir .gt. n ) go to 70
      icr = ic + w(ir)
      ipr = ip - p(ir)
      do 40 i=jbr,nnfo
        j = ff(i)
        if ( w(j) .gt. icr ) go to 50
        icr = icr - w(j)
        ipr = ipr + p(j)
   40 continue
      j = ff(nnfo)
   50 iub = ipr + icr*p(j)/w(j)
      if ( iub .le. izc ) go to 60
c
c item ir cannot be fixed to 1.
c
      nnf = nnf + 1
      ff(nnf) = ir
      iz1 = iz1 - p(ir)
      icw = icw + w(ir)
   60 ir = nf(ir)
      go to 30
c
c reduce the items in n0.
c
   70 ir = fn0
   80 if ( ir .gt. n ) return
      if ( w(ir) .gt. icw ) go to 120
      icr = ic - w(ir)
      ipr = ip + p(ir)
      i = jbr
      j = ff(i)
   90 if ( icr .ge. 0 ) go to 110
      i = i - 1
      if ( i .eq. 0 ) go to 100
      j = ff(i)
      icr = icr + w(j)
      ipr = ipr - p(j)
      go to 90
  100 j = ff(1)
  110 iub = ipr + icr*p(j)/w(j)
      if ( iub .le. izc ) go to 120
c
c item ir cannot be fixed to 0.
c
      nnf = nnf + 1
      ff(nnf) = ir
  120 ir = nf(ir)
      go to 80
      end
      subroutine reds(n,ps,ws,p,w,c,ia1,np1,nnf,x,iz1,izh,icw,ia2)

c*********************************************************************72
c
cc REDS reduces the original problem.
c
c  Discussion:
c
c    It is assumed that the items are sorted
c    according to decreasing profit per unit weight.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer ps(np1),ws(np1),p(n),w(n),x(n),ia1(n),ia2(n)
      integer c,cwr,ap,ar,fr,r,sw
c
c initialize.
c
      nnf = 0
      ip = 0
      cwr = c
      j = 1
   10 if ( cwr .le. ws(j) ) go to 20
      cwr = cwr - ws(j)
      ip = ip + ps(j)
      j = j + 1
      go to 10
   20 r = j
      jbr = ia1(r)
      if ( cwr .lt. ws(j) ) go to 50
      izh = ip + ps(r)
      do j=1,r
        jj = ia1(j)
        x(jj) = 1
      end do
      ir = r + 1
      do j=ir,n
        jj = ia1(j)
        x(jj) = 0
      end do
      return
c
c compute the upper bound.
c
   50 ll = j - 1
      ps(np1) = 0
      ws(np1) = c + 1
      iub = ip + cwr*ps(ll+2)/ws(ll+2)
      a = ws(ll+1) - cwr
      iub1 = float(ip + ps(ll+1)) - a*float(ps(ll))/float(ws(ll))
      if ( iub1 .gt. iub ) iub = iub1
c
c greedy solution.
c
      izg = ip
      icwr = cwr
      ig = ll
      do 60 i=j,n
        if ( ws(i) .gt. icwr ) go to 60
        ig = i
        izg = izg + ps(i)
        icwr = icwr - ws(i)
   60 continue
      if ( izg .lt. iub ) go to 120
c
c determine the greedy solution (optimal).
c
      izh = izg

      do i=1,ll
        ii = ia1(i)
        x(ii) = 1
      end do

      icw = cwr
      if ( j .gt. ig ) go to 100
      do 90 i=j,ig
        ii = ia1(i)
        if ( ws(i) .gt. icw ) go to 80
        x(ii) = 1
        icw = icw - ws(i)
        go to 90
   80   x(ii) = 0
   90 continue
  100 if ( ig .eq. n ) return
      ig1 = ig + 1
      do 110 i=ig1,n
        ii = ia1(i)
        x(ii) = 0
  110 continue
      return
  120 izh = izg
c
c compute f(i) for i .le. r .
c
      jflag = 0
      do 150 i=1,r
        ar = cwr + ws(i)
        ap = ip - ps(i)
        k = r
  130   if ( ar .lt. ws(k) ) go to 140
        ar = ar - ws(k)
        ap = ap + ps(k)
        k = k + 1
        go to 130
  140   ii = ia1(i)
        x(ii) = ap + ar*ps(k)/ws(k)
        if ( izh .ge. ap ) go to 150
        izh = ap
        if ( izh .eq. iub ) go to 280
  150 continue
      fr = x(jbr)
c
c compute f(i) for i .ge. r .
c
      jflag = 1
      do 180 i=r,n
        ar = cwr - ws(i)
        ap = ip + ps(i)
        k = r
  160   if ( ar .ge. 0 ) go to 170
        k = k - 1
        ar = ar + ws(k)
        ap = ap - ps(k)
        go to 160
  170   ii = ia1(i)
        x(ii) = ap + ar*ps(k)/ws(k)
        if ( izh .ge. ap ) go to 180
        izh = ap
        if ( izh .eq. iub ) go to 280
  180 continue
c
c try to insert items in the solution.
c
      jr = r - 1
      icw = c
      sw = 0
      iz1 = 0
      do 200 i=1,jr
        ii = ia1(i)
        if ( x(ii) .lt. izh ) go to 190
        x(ii) = 2
        nnf = nnf + 1
        ia2(nnf) = ii
        sw = sw + ws(i)
        go to 200
  190   x(ii) = 1
        icw = icw - ws(i)
        iz1 = iz1 + ps(i)
  200 continue
      ir = r
      if ( fr .ge. izh ) go to 210
      x(jbr) = 1
      icw = icw - ws(r)
      iz1 = iz1 + ps(r)
      ir = r + 1
      if ( ir .gt. n ) go to 240
  210 do 230 i=ir,n
        ii = ia1(i)
        if ( x(ii) .lt. izh ) go to 220
        x(ii) = 2
        nnf = nnf + 1
        ia2(nnf) = ii
        sw = sw + ws(i)
        go to 230
  220   x(ii) = 0
  230 continue
  240 if ( nnf .eq. 0 ) return
      nnn = nnf
      nnf = 0
      do 260 i=1,nnn
        ii = ia2(i)
        if ( w(ii) .le. icw ) go to 250
        x(ii) = 0
        sw = sw - w(ii)
        go to 260
  250   nnf = nnf + 1
        ia2(nnf) = ii
  260 continue
      if ( icw .lt. sw ) return
      if ( nnf .eq. 0 ) return
      do 270 i=1,nnf
        ii = ia2(i)
        x(ii) = 1
        iz1 = iz1 + p(ii)
  270 continue
      izh = iz1
      nnf = 0
      return
c
c determine the heuristic solution (optimal).
c
  280 if ( k .eq. 1 ) go to 300
      k1 = k - 1
      do j=1,k1
        jj = ia1(j)
        x(jj) = 1
      end do

  300 if ( k .gt. n ) go to 320

      do j=k,n
        jj = ia1(j)
        x(jj) = 0
      end do

  320 x(ii) = jflag
      return
      end
      subroutine redu(n,po,wo,jdim,jpx,x)

c*********************************************************************72
c
cc REDU reduces an unbounded knapsack problem (po,wo) through dominance relations.
c
c on output, jpx is the first undominated item, x(jpx) the second, and
c so on. if y is the last one, then x(y) = 0.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer po(jdim),wo(jdim),x(jdim)
      integer feq,prfeq,prj,prk
c
c initialization.
c
      jpx = 1
      do j=1,n
        x(j) = j + 1
      end do
      x(n) = 0
      crat = float(po(1))/float(wo(1)) + 1.
      prfeq = 0
      prj = 0
c
c main iteration.
c
      j = jpx
   20   iwoj = wo(j)
        ipoj = po(j)
        rj = float(ipoj)/float(iwoj)
        if ( rj .eq. crat ) go to 30
        crat = rj
        prfeq = prj
        feq = j
        go to 80
c
c items k preceding j with same ratio.
c
   30   k = feq
        prk = prfeq
   40     if ( (wo(k)/iwoj)*ipoj .lt. po(k) ) go to 60
c
c item j dominates item k.
c
          if ( prk .eq. 0 ) go to 50
          x(prk) = x(k)
          go to 70
   50     jpx = x(k)
          go to 70
   60     prk = k
   70     k = x(k)
        if ( k .lt. j ) go to 40
c
c items k following j.
c
   80   k = x(j)
        prk = j
   90     if ( k .eq. 0 ) go to 120
          if ( (wo(k)/iwoj)*ipoj .lt. po(k) ) go to 100
c
c item j dominates item k.
c
          x(prk) = x(k)
          go to 110
  100     prk = k
  110     k = x(k)
        go to 90
  120   prj = j
        j = x(j)
      if ( j .gt. 0 ) go to 20
      return
      end
      subroutine restor(k,zz,n,c,kfix,fixit,w,x,r,lastw,jdim)

c*********************************************************************72
c
cc RESTOR restores the situation preceding the reduction of level  k and update  lastw .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer kfix(jdim),fixit(jdim),w(jdim),x(jdim),r(jdim)
      integer zz,c
      i = kfix(k)
      kfix(k) = 0
   10 next = fixit(i)
      fixit(i) = 0
      j = x(i)
      r(j) = r(j) + w(i)
      if ( next .eq. (- 1) ) go to 20
      i = next
      go to 10
   20 if ( r(zz) .lt. c ) go to 30
      zz = zz - 1
      go to 20
   30 do 40 ii=1,n
        i = n - ii + 1
        if ( fixit(i) .eq. 0 ) go to 50
   40 continue
c
c no exit this way.
c
   50 lastw = w(i)
      return
      end
      subroutine search(n,w,r,nl,jdim)

c*********************************************************************72
c
cc SEARCH finds the largest NL such that R < W(NL).
c
c given  w(1),...,w(n) , sorted by decreasing values, find, through
c binary search, the largest  nl  such that  w(nl) .gt. r .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer w(jdim)
      n1 = 1
      n2 = n
   10 if ( n2 - n1 .le. 2 ) go to 30
      nl = (n1 + n2)/2
      if ( float(w(nl)) .gt. r ) go to 20
      n2 = nl - 1
      go to 10
   20 n1 = nl + 1
      go to 10
   30 do 40 i=n1,n2
        if ( float(w(i)) .le. r ) go to 50
   40 continue
      nl = n2
      return
   50 nl = i - 1
      return
      end
      subroutine sigma(n,m,p,w,q,i,b,kub,ub)

c*********************************************************************72
c
cc SIGMA computes an upper bound  ub  on the best final solution which
c can be obtained from the current solution.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer       p(1000),w(1000),q(10),b(1001),ub
      integer       bs,ps,qs,sb,ws,xs
      common /sngl/ bs(1000),ps(1001),ws(1001),xs(1000)
      common /pub/  lx(1000),lxi(1000),lr,lri,lubi
      ns = 0

      qs = 0
      do j=i,m
        qs = qs + q(j)
      end do

      sb = 0
      do 20 j=1,n
        lx(j) = 0
        if ( b(j) .eq. 0 ) go to 20
        ns = ns + 1
        bs(ns) = j
        ps(ns) = p(j)
        ws(ns) = w(j)
        sb = sb + w(j)
   20 continue
      if ( sb .gt. qs ) go to 40
      lr = qs - sb
      ub = 0
      if ( ns .eq. 0 ) return
      do 30 j=1,ns
        ub = ub + ps(j)
        xs(j) = 1
   30 continue
      go to 50
   40 call skp2(ns,ps,ws,qs,kub,xs,ub)
      lr = qs
   50 do j=1,ns
        jj = bs(j)
        lx(jj) = xs(j)
      end do

      return
      end
      subroutine skp1(n,p,w,kqi,vsti,i,kqri,b,a,x,y,u,jstep,
     &               jdimr,jdimc,jdimc1,kpunt,jp,kx,kp,kw,r,dmyc1,
     &               dmyc2,dmyc3,dmyc4,dmyc5)

c*********************************************************************72
c
cc SKP1 solves a 0-1 single knapsack problem.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),b(jdimc),
     &        a(jdimr,jdimc),u(jdimc),vsti,x(jdimr,jdimc),y(jdimc)
      integer kpunt(jdimc),jp(jdimc),kp(jdimc1),kw(jdimc1),kx(jdimc)
      integer dmyc1(jdimc),dmyc2(jdimc),dmyc3(jdimc),dmyc4(jdimc),
     &        dmyc5(jdimc)
      real    r(jdimc)
      kn = 0
      ksw = 0
      do 10 j=1,n
        if ( b(j) .le. 0 ) go to 10
        if ( a(i,j) .ne. 0 ) go to 10
        if ( w(i,j) .gt. kqi ) go to 10
        kkp = p(i,j) - u(j)
        if ( kkp .le. 0 ) go to 10
        kn = kn + 1
        r(kn) = float(kkp)/float(w(i,j))
        jp(kn) = kn
        kpunt(kn) = j
        ksw = ksw + w(i,j)
   10 continue

      if ( jstep .ne. 4 ) go to 30

      do j=1,n
        y(j) = 0
      end do

   30 kqri = kqi
      if ( kn .eq. 0 ) return
      if ( ksw .gt. kqi ) go to 50

      do kj=1,kn
        j = kpunt(kj)
        vsti = vsti + (p(i,j) - u(j))
        kqri = kqri - w(i,j)
        if ( jstep .eq. 2 ) x(i,j) = 1
        if ( jstep .eq. 4 ) y(j) = 1
      end do

      return
   50 call sortr(kn,r,jp,jdimc)

      do kj=1,kn
        k = jp(kj)
        j = kpunt(k)
        kp(kj) = p(i,j) - u(j)
        kw(kj) = w(i,j)
        jp(kj) = j
      end do

      mubf = 0
      jvsti = - 1
      call kpmax(kn,kp,kw,kqi,jvsti,kx,mubf,jdimc+1,jdimc,
     &           dmyc1,dmyc2,dmyc3,dmyc4,dmyc5)
      vsti = vsti + jvsti
      kqri = kqi

      do kj=1,kn
        j = jp(kj)
        if ( jstep .eq. 2 ) x(i,j) = kx(kj)
        if ( jstep .eq. 4 ) y(j) = kx(kj)
        kqri = kqri - kx(kj)*w(i,j)
      end do

      return
      end
      subroutine skp2(ns,ps,ws,qs,kub,xs,vs)

c*********************************************************************72
c
cc SKP2 solves the 0-1 single knapsack problem
c
c maximize    vs = ps(1)*xs(1) + ... + ps(ns)*xs(ns)
c subject to:      ws(1)*xs(1) + ... + ws(ns)*xs(ns) .le. qs ,
c                  xs(j) = 0 or 1  for j=1,...,ns,
c                  vs .gt. kub .
c
c this subroutine is a modified version of subroutine mt1.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer ps(1001),ws(1001),xs(1000),qs,vs
      integer d(1000),min(1000),pbar(1000),wbar(1000),zbar(1000)
      integer diff,pr,r,t
      vs = kub
      ip = 0
      ms = qs
      do l=1,ns
        ll = l
        if ( ws(l) .gt. ms ) go to 20
        ip = ip + ps(l)
        ms = ms - ws(l)
      end do

   20 ll = ll - 1
      if ( ms .eq. 0 ) go to 50
      ps(ns+1) = 0
      ws(ns+1) = qs + 1
      lim = ip + ms*ps(ll+2)/ws(ll+2)
      a = ip + ps(ll+1)
      b = (ws(ll+1) - ms)*ps(ll)
      c = ws(ll)
      lim1 = a - b/c
      if ( lim1 .gt. lim ) lim = lim1
      if ( lim .le. vs ) return
      mink = qs + 1
      min(ns) = mink

      do j=2,ns
        kk = ns + 2 - j
        if ( ws(kk) .lt. mink ) mink = ws(kk)
        min(kk-1) = mink
      end do

      do j=1,ns
        d(j) = 0
      end do

      pr = 0
      lold = ns
      ii = 1
      go to 170
   50 if ( vs .ge. ip ) return
      vs = ip

      do j=1,ll
        xs(j) = 1
      end do

      nn = ll + 1

      do j=nn,ns
        xs(j) = 0
      end do

      qs = 0
      return
   80 if ( ws(ii) .le. qs ) go to 90
      ii1 = ii + 1
      if ( vs .ge. qs*ps(ii1)/ws(ii1) + pr ) go to 280
      ii = ii1
      go to 80
   90 ip = pbar(ii)
      ms = qs - wbar(ii)
      in = zbar(ii)
      ll = ns
      if ( in .gt. ns ) go to 110
      do 100 l=in,ns
        ll = l
        if ( ws(l) .gt. ms ) go to 160
        ip = ip + ps(l)
        ms = ms - ws(l)
  100 continue
  110 if ( vs .ge. ip + pr ) go to 280
      vs = ip + pr
      mfirst = ms
      nn = ii - 1
      do 120 j=1,nn
        xs(j) = d(j)
  120 continue
      do 130 j=ii,ll
        xs(j) = 1
  130 continue
      if ( ll .eq. ns ) go to 150
      nn = ll + 1
      do 140 j=nn,ns
        xs(j) = 0
  140 continue
  150 if ( vs .ne. lim ) go to 280
      qs = mfirst
      return
  160 l = ll
      ll = ll - 1
      if ( ms .eq. 0 ) go to 110
      if ( vs .ge. pr + ip + ms*ps(l)/ws(l) ) go to 280
  170 wbar(ii) = qs - ms
      pbar(ii) = ip
      zbar(ii) = ll + 1
      d(ii) = 1
      nn = ll - 1

      do j=ii,nn
        wbar(j+1) = wbar(j) - ws(j)
        pbar(j+1) = pbar(j) - ps(j)
        zbar(j+1) = ll + 1
        d(j+1) = 1
      end do

      j1 = ll + 1

      do j=j1,lold
        wbar(j) = 0
        pbar(j) = 0
        zbar(j) = j
      end do

      lold = ll
      qs = ms
      pr = pr + ip
      if ( ll - (ns - 2) ) 240, 220, 210
  210 ii = ns
      go to 250
  220 if ( qs .lt. ws(ns) ) go to 230
      qs = qs - ws(ns)
      pr = pr + ps(ns)
      d(ns) = 1
  230 ii = ns - 1
      go to 250
  240 ii = ll + 2
      if ( qs .ge. min(ii-1) ) go to 80
  250 if ( vs .ge. pr ) go to 270
      vs = pr
      do 260 j=1,ns
        xs(j) = d(j)
  260 continue
      mfirst = qs
      if ( vs .eq. lim ) return
  270 if ( d(ns) .eq. 0 ) go to 280
      d(ns) = 0
      qs = qs + ws(ns)
      pr = pr - ps(ns)
  280 nn = ii - 1
      if ( nn .eq. 0 ) go to 300
      do 290 j=1,nn
        kk = ii - j
        if ( d(kk) .eq. 1 ) go to 310
  290 continue
  300 qs = mfirst
      return
  310 r = qs
      qs = qs + ws(kk)
      pr = pr - ps(kk)
      d(kk) = 0
      if ( r .lt. min(kk) ) go to 320
      ii = kk + 1
      go to 80
  320 nn = kk + 1
      ii = kk
  330 if ( vs .ge. pr + qs*ps(nn)/ws(nn) ) go to 280
      diff = ws(nn) - ws(kk)
      if ( diff ) 390, 340, 350
  340 nn = nn + 1
      go to 330
  350 if ( diff .gt. r ) go to 340
      if ( vs .ge. pr + ps(nn) ) go to 340
      vs = pr + ps(nn)
      do 360 j=1,kk
        xs(j) = d(j)
  360 continue
      jj = kk + 1
      do 370 j=jj,ns
        xs(j) = 0
  370 continue
      xs(nn) = 1
      mfirst = qs - ws(nn)
      if ( vs .ne. lim ) go to 380
      qs = mfirst
      return
  380 r = r - diff
      kk = nn
      nn = nn + 1
      go to 330
  390 t = r - diff
      if ( t .lt. min(nn) ) go to 340
      n = nn + 1
      if ( vs .ge. pr + ps(nn) + t*ps(n)/ws(n) ) go to 280
      qs = qs - ws(nn)
      pr = pr + ps(nn)
      d(nn) = 1
      ii = nn + 1
      wbar(nn) = ws(nn)
      pbar(nn) = ps(nn)
      zbar(nn) = ii
      n1 = nn + 1

      do j=n1,lold
        wbar(j) = 0
        pbar(j) = 0
        zbar(j) = j
      end do

      lold = nn
      go to 80
      end
      subroutine sol(n,b,xt,jdim1,jdim2,x)

c*********************************************************************72
c
cc SOL determines the solution vector x for the original problem.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer b(jdim1),xt(jdim2),x(jdim1)
      nt = 0
      do 20 j=1,n
        isum = 0
        id = 1
        x(j) = 0
   10   nt = nt + 1
        isum = isum + id
        x(j) = x(j) + id*xt(nt)
        id = id*2
        if ( id + isum .le. b(j) ) go to 10
        if ( isum .eq. b(j) ) go to 20
        id = b(j) - isum
        nt = nt + 1
        x(j) = x(j) + id*xt(nt)
   20 continue
      return
      end
      subroutine sort7(na,a,i)

c*********************************************************************72
c
cc SORT7 sorts in increasing order the elements from
c a(i)  to  a(i+6)  of  a  by performing at most  13  tests.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      dimension a(na)
      i1 = i + 1
      i2 = i + 2
      i3 = i + 3
      i4 = i + 4
      i5 = i + 5
      i6 = i + 6
      if ( a(i) .gt. a(i1) ) go to 10
      a1 = a(i)
      a2 = a(i1)
      go to 20
   10 a1 = a(i1)
      a2 = a(i)
   20 if ( a(i2) .gt. a(i3) ) go to 30
      a3 = a(i2)
      a4 = a(i3)
      go to 40
   30 a3 = a(i3)
      a4 = a(i2)
   40 if ( a1 .gt. a3 ) go to 50
      a5 = a2
      a2 = a3
      a3 = a4
      go to 60
   50 a5 = a4
      aux = a3
      a3 = a2
      a2 = a1
      a1 = aux
   60 a4 = a(i4)
      if ( a4 .ge. a2 ) go to 80
      if ( a4 .ge. a1 ) go to 70
      a4 = a3
      a3 = a2
      a2 = a1
      a1 = a(i4)
      go to 90
   70 a4 = a3
      a3 = a2
      a2 = a(i4)
      go to 90
   80 if ( a4 .ge. a3 ) go to 90
      a4 = a3
      a3 = a(i4)
   90 a(i) = a1
      if ( a5 .gt. a3 ) go to 110
      a(i3) = a3
      a(i4) = a4
      if ( a5 .gt. a2 ) go to 100
      a(i1) = a5
      a(i2) = a2
      go to 130
  100 a(i1) = a2
      a(i2) = a5
      go to 130
  110 a(i1) = a2
      a(i2) = a3
      if ( a5 .gt. a4 ) go to 120
      a(i3) = a5
      a(i4) = a4
      go to 130
  120 a(i3) = a4
      a(i4) = a5
  130 a5 = a(i5)
      if ( a5 .lt. a(i2) ) go to 150
      if ( a5 .lt. a(i3) ) go to 140
      if ( a5 .ge. a(i4) ) go to 180
      a(i5) = a(i4)
      a(i4) = a5
      go to 180
  140 a(i5) = a(i4)
      a(i4) = a(i3)
      a(i3) = a5
      go to 180
  150 a(i5) = a(i4)
      a(i4) = a(i3)
      a(i3) = a(i2)
      if ( a5 .lt. a(i1) ) go to 160
      a(i2) = a5
      go to 180
  160 a(i2) = a(i1)
      if ( a5 .lt. a(i) ) go to 170
      a(i1) = a5
      go to 180
  170 a(i1) = a(i)
      a(i) = a5
  180 a6 = a(i6)
      if ( a6 .lt. a(i3) ) go to 200
      if ( a6 .lt. a(i4) ) go to 190
      if ( a6 .ge. a(i5) ) return
      a(i6) = a(i5)
      a(i5) = a6
      return
  190 a(i6) = a(i5)
      a(i5) = a(i4)
      a(i4) = a6
      return
  200 a(i6) = a(i5)
      a(i5) = a(i4)
      a(i4) = a(i3)
      if ( a6 .lt. a(i1) ) go to 220
      if ( a6 .lt. a(i2) ) go to 210
      a(i3) = a6
      return
  210 a(i3) = a(i2)
      a(i2) = a6
      return
  220 a(i3) = a(i2)
      a(i2) = a(i1)
      if ( a6 .lt. a(i) ) go to 230
      a(i1) = a6
      return
  230 a(i1) = a(i)
      a(i) = a6
      return
      end
      subroutine sorti(n,a,v,jda)

c*********************************************************************72
c
cc SORTI sorts the integer array a by decreasing values (derived from
c subroutine sortzv of the c.e.r.n. library).
c
c jda           = length of array a;
c n             = number of elements of a to be sorted;
c v(i) (input)  = pointer to the i-th element to be sorted;
c v(i) (output) = pointer to the i-th element of the sorted array.
c
c on return, array a is unchanged.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer v(n),iu(20),il(20)
      integer a(jda),t
      ii = 1
      jj = n
      if ( n .le. 1 ) return
      m = 1
      i = ii
      j = jj
   10 if ( i .ge. j ) go to 80
   20 k = i
      ij = (j + i)/2
      iv = v(ij)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .ge. t ) go to 30
      v(ij) = ki
      v(i) = iv
      iv = v(ij)
      t = a(iv)
   30 l = j
      ki = v(j)
      if ( a(ki) .le. t ) go to 50
      v(ij) = ki
      v(j) = iv
      iv = v(ij)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .ge. t ) go to 50
      v(ij) = ki
      v(i) = iv
      iv = v(ij)
      t = a(iv)
      go to 50
   40 v(l) = v(k)
      v(k) = ivt
   50 l = l - 1
      ki = v(l)
      if ( a(ki) .lt. t ) go to 50
      ivt = ki
   60 k = k + 1
      ki = v(k)
      if ( a(ki) .gt. t ) go to 60
      if ( k .le. l ) go to 40
      if ( l - i .le. j - k ) go to 70
      il(m) = i
      iu(m) = l
      i = k
      m = m + 1
      go to 90
   70 il(m) = k
      iu(m) = j
      j = l
      m = m + 1
      go to 90
   80 m = m - 1
      if ( m .eq. 0 ) return
      i = il(m)
      j = iu(m)
   90 if ( j - i .ge. ii ) go to 20
      if ( i .eq. ii ) go to 10
      i = i - 1
  100 i = i + 1
      if ( i .eq. j ) go to 80
      iv = v(i+1)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .ge. t ) go to 100
      k = i
  110 v(k+1) = v(k)
      k = k - 1
      ki = v(k)
      if ( t .gt. a(ki) ) go to 110
      v(k+1) = iv
      go to 100
      end
      subroutine sorti2(n,a,v,jda)

c*********************************************************************72
c
cc SORTI2 index-sorts the integer array a by increasing values.
c
c jda           = length of array a;
c n             = number of elements of a to be sorted;
c v(i) (input)  = pointer to the i-th element to be sorted;
c v(i) (output) = pointer to the i-th element of the sorted array.
c
c on return, array a is unchanged.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer v(n),iu(20),il(20)
      integer a(jda),t
      ii = 1
      jj = n
      if ( n .le. 1 ) return
      m = 1
      i = ii
      j = jj
   10 if ( i .ge. j ) go to 80
   20 k = i
      ij = (j + i)/2
      iv = v(ij)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .le. t ) go to 30
      v(ij) = ki
      v(i) = iv
      iv = v(ij)
      t = a(iv)
   30 l = j
      ki = v(j)
      if ( a(ki) .ge. t ) go to 50
      v(ij) = ki
      v(j) = iv
      iv = v(ij)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .le. t ) go to 50
      v(ij) = ki
      v(i) = iv
      iv = v(ij)
      t = a(iv)
      go to 50
   40 v(l) = v(k)
      v(k) = ivt
   50 l = l - 1
      ki = v(l)
      if ( a(ki) .gt. t ) go to 50
      ivt = ki
   60 k = k + 1
      ki = v(k)
      if ( a(ki) .lt. t ) go to 60
      if ( k .le. l ) go to 40
      if ( l - i .le. j - k ) go to 70
      il(m) = i
      iu(m) = l
      i = k
      m = m + 1
      go to 90
   70 il(m) = k
      iu(m) = j
      j = l
      m = m + 1
      go to 90
   80 m = m - 1
      if ( m .eq. 0 ) return
      i = il(m)
      j = iu(m)
   90 if ( j - i .ge. ii ) go to 20
      if ( i .eq. ii ) go to 10
      i = i - 1
  100 i = i + 1
      if ( i .eq. j ) go to 80
      iv = v(i+1)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .le. t ) go to 100
      k = i
  110 v(k+1) = v(k)
      k = k - 1
      ki = v(k)
      if ( t .lt. a(ki) ) go to 110
      v(k+1) = iv
      go to 100
      end
      subroutine sortr(n,a,v,jda)

c*********************************************************************72
c
cc SORTR index-sorts the real array a by decreasing values.
c
c jda           = length of array a;
c n             = number of elements of a to be sorted;
c v(i) (input)  = pointer to the i-th element to be sorted;
c v(i) (output) = pointer to the i-th element of the sorted array.
c
c on return, array a is unchanged.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer v(n),iu(20),il(20)
      real    a(jda)
      ii = 1
      jj = n
      if ( n .le. 1 ) return
      m = 1
      i = ii
      j = jj
   10 if ( i .ge. j ) go to 80
   20 k = i
      ij = (j + i)/2
      iv = v(ij)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .ge. t ) go to 30
      v(ij) = ki
      v(i) = iv
      iv = v(ij)
      t = a(iv)
   30 l = j
      ki = v(j)
      if ( a(ki) .le. t ) go to 50
      v(ij) = ki
      v(j) = iv
      iv = v(ij)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .ge. t ) go to 50
      v(ij) = ki
      v(i) = iv
      iv = v(ij)
      t = a(iv)
      go to 50
   40 v(l) = v(k)
      v(k) = ivt
   50 l = l - 1
      ki = v(l)
      if ( a(ki) .lt. t ) go to 50
      ivt = ki
   60 k = k + 1
      ki = v(k)
      if ( a(ki) .gt. t ) go to 60
      if ( k .le. l ) go to 40
      if ( l - i .le. j - k ) go to 70
      il(m) = i
      iu(m) = l
      i = k
      m = m + 1
      go to 90
   70 il(m) = k
      iu(m) = j
      j = l
      m = m + 1
      go to 90
   80 m = m - 1
      if ( m .eq. 0 ) return
      i = il(m)
      j = iu(m)
   90 if ( j - i .ge. ii ) go to 20
      if ( i .eq. ii ) go to 10
      i = i - 1
  100 i = i + 1
      if ( i .eq. j ) go to 80
      iv = v(i+1)
      t = a(iv)
      ki = v(i)
      if ( a(ki) .ge. t ) go to 100
      k = i
  110 v(k+1) = v(k)
      k = k - 1
      ki = v(k)
      if ( t .gt. a(ki) ) go to 110
      v(k+1) = iv
      go to 100
      end
      subroutine tab(tda,tdb,nsd,wk,c,jddx,jdd,jbit,jflag)

c*********************************************************************72
c
cc TAB builds the new dynamic programming list TDB from the current list TDA.
c
c  Discussion:
c
c    The routine adds the states which can be obtained from  wk .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer wk,c,tda(jdd,2),tdb(jdd,2)
      jflag = 0
      tda(nsd+1,1) = 2*c + 1
      ka = 1
      kas = 1
      kb = 0
      newst = wk
      newsol = jbit
   10 if ( newst - tda(ka,1) ) 20 , 30 , 40
   20 kb = kb + 1
      if ( kb .gt. jddx ) go to 60
      tdb(kb,1) = newst
      tdb(kb,2) = newsol
   30 if ( kas .eq. nsd ) go to 50
      kas = kas + 1
      newst = wk + tda(kas,1)
      newsol = jbit + tda(kas,2)
      if ( newst .le. c ) go to 10
      if ( ka .gt. nsd ) go to 50
      go to 10
   40 kb = kb + 1
      if ( kb .gt. jddx ) go to 60
      tdb(kb,1) = tda(ka,1)
      tdb(kb,2) = tda(ka,2)
      ka = ka + 1
      go to 10
   50 nsd = kb
      if ( tdb(nsd,1) .gt. c ) nsd = nsd - 1
      go to 70
   60 jflag = - 1
   70 continue
      return
      end
      subroutine termin(jfi,invst,jub,imult,z,kvst,numnod,minmax,
     &                  m,n,p,lam,jdimr,jdimc,jb,back)

c*********************************************************************72
c
cc TERMIN terminates execution.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer z,p(jdimr,jdimc),back
      integer zm
      if ( jfi .eq. 1 ) go to 10
      if ( jub .lt. z ) jub = z
      jb = invst - jub*imult
      if ( z .eq. (-7) ) go to 10
      zm = z
      z = 0
      if ( zm .gt. kvst ) z = invst - zm*imult
      back = numnod
   10 if ( minmax .eq. 2 ) return
c
c  Restore the original minimization problem.
c
      do i=1,m
        do j=1,n
          if ( p(i,j) .gt. 0 ) p(i,j) = lam - p(i,j)
        end do
      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
      subroutine trans(n,p,w,b,jdim1,jdim2,nt,pt,wt)

c*********************************************************************72
c
cc TRANS transforms a bounded knapsack problem (n, p, w, b) into
c a 0-1 knapsack problem (nt, pt, wt ).
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdim1),w(jdim1),b(jdim1),pt(jdim2),wt(jdim2)
      jdmax = jdim2 - 3
      nt = 0
      do 20 j=1,n
        isum = 0
        id = 1
   10   nt = nt + 1
        if ( nt .gt. jdmax ) go to 30
        pt(nt) = p(j)*id
        wt(nt) = w(j)*id
        isum = isum + id
        id = id*2
        if ( id + isum .le. b(j) ) go to 10
        if ( isum .eq. b(j) ) go to 20
        id = b(j) - isum
        nt = nt + 1
        if ( nt .gt. jdmax ) go to 30
        pt(nt) = p(j)*id
        wt(nt) = w(j)*id
   20 continue
      return
   30 nt = - 5
      return
      end
      subroutine trin(p,n,m,invst,lam,jdimr,jdimc)

c*********************************************************************72
c
cc TRIN transforms an gap in minimization form to an equivalent instance in maximization form.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc)
      invst = 0
      max = 0
      do 20 j=1,n
        do i=1,m
          if ( p(i,j) .gt. max ) max = p(i,j)
        end do
   20 continue
      lam = max + 1
      do 40 j=1,n
        do 30 i=1,m
          p(i,j) = lam - p(i,j)
   30   continue
        invst = invst + lam
   40 continue
      return
      end
      subroutine ubfjv(n,m,p,w,q,b,a,jfjvu,xrs,vfjv,vstar,inf,
     &                 jdimr,jdimc,jdimc1,dmyc1,dmyc2,dmyc3,dmycc1,
     &                 dmycc2,dmycr1,dmyc4,dmyc5,dmyc6,dmyc7,dmyc8,
     &                 u,nots,y,vst,kq,kqr,miny,x)

c*********************************************************************72
c
cc UBFJV computes the fisher-jaikumar-van wassenhove upper bound.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),q(jdimr),b(jdimc),
     &        a(jdimr,jdimc),xrs(jdimc)
      integer vstar,vfjv
      integer u(jdimc),vst(jdimr),kq(jdimr),kqr(jdimr),
     &        nots(jdimc),y(jdimc),delta,vsti
      integer x(jdimr,jdimc),miny(jdimc)
      integer dmyc1(jdimc),dmyc2(jdimc),dmyc3(jdimc),dmyc4(jdimc),
     &        dmyc5(jdimc),dmyc6(jdimc),dmyc7(jdimc),dmyc8(jdimc),
     &        dmycc1(jdimc1),dmycc2(jdimc1)
      real    dmycr1(jdimc)
c
c step 1.
c
      vfjv = 0

      do i=1,m
        kq(i) = q(i)
        vst(i) = 0
        do j=1,n
          x(i,j) = 0
        end do
      end do

      jfjvu = 0

      do 50 j=1,n
        u(j) = 0
        if ( b(j) .eq. 1 ) go to 50
        do i=1,m
          if ( a(i,j) .eq. 1 ) go to 40
        end do
   40   jfjvu = jfjvu + p(i,j)
   50 continue

   60 jrep = 0
      do 110 j=1,n
        if ( b(j) .le. 0 ) go to 110
        max = - 1
        max2 = - 1
        do 80 i=1,m
          if ( a (i,j) .eq. (- 1) ) go to 80
          if ( w(i,j) .gt. kq(i) ) go to 80
          if ( p(i,j) .le. max2 ) go to 80
          if ( p(i,j) .gt. max ) go to 70
          max2 = p(i,j)
          go to 80
   70     max2 = max
          max = p(i,j)
          imax = i
   80   continue
        jfjvu = jfjvu - u(j)
        u(j) = max2
        if ( max .gt. 0 ) go to 90
        jfjvu = - 1
        go to 360
   90   if ( max2 .le. 0 ) go to 100
        jfjvu = jfjvu + u(j)
        go to 110
  100   kq(imax) = kq(imax) - w(imax,j)
        jfjvu = jfjvu + p(imax,j)
        b(j) = - imax
        jrep = 1
  110 continue
      if ( jrep .eq. 1 ) go to 60
c
c step 2.
c
      do 120 i=1,m
        call skp1(n,p,w,kq(i),vst(i),i,kqr(i),b,a,x,y,u,2,
     &           jdimr,jdimc,jdimc+1,dmyc1,dmyc2,dmyc3,dmycc1,dmycc2,
     &           dmycr1,dmyc4,dmyc5,dmyc6,dmyc7,dmyc8)
        jfjvu = jfjvu + vst(i)
  120 continue
c
c step 3.
c
  130 if ( jfjvu .le. vstar ) go to 360
c
c initialize nots.
c
      jns = 0
      do 150 j=1,n
        if ( b(j) .le. 0 ) go to 150
        do 140 i=1,m
          if ( x(i,j) .eq. 1 ) go to 150
  140   continue
        jns = jns + 1
        nots(jns) = j
  150 continue
c
c iterative part.
c
  160 if ( jns .gt. 0 ) go to 240
      do 230 j=1,n
        if ( b(j) .eq. 0 ) go to 200
        if ( b(j) .gt. 0 ) go to 170
        i = - b(j)
        go to 190
  170   do 180 i=1,m
          if ( x(i,j) .eq. 1 ) go to 190
  180   continue
  190   xrs(j) = i
        vfjv = vfjv + p(i,j)
        go to 230
  200   do 210 i=1,m
          if ( a(i,j) .eq. 1 ) go to 220
  210   continue
  220   xrs(j) = i
        vfjv = vfjv + p(i,j)
  230 continue
      go to 360
  240 max = - 1
      do 260 jj=1,jns
        j = nots(jj)
        do 250 i=1,m
          if ( p(i,j) - u(j) .ne. 0 ) go to 250
          if ( w(i,j) .gt. kqr(i) ) go to 250
          if ( p(i,j) .le. max ) go to 250
          max = p(i,j)
          imax = i
          jmax = j
          jjmax = jj
  250   continue
  260 continue
      if ( max .lt. 0 ) go to 270
      x(imax,jmax) = 1
      kqr(imax) = kqr(imax) - w(imax,jmax)
      nots(jjmax) = nots(jns)
      jns = jns - 1
      go to 160
c
c step 4.
c
  270 do 330 jj=1,jns
        j = nots(jj)
        min = inf
        min2 = min
        do 300 i=1,m
          if ( a(i,j) .ne. 0 ) go to 300
          if ( w(i,j) .gt. kq(i) ) go to 300
          a(i,j) = - 2
          kqi = kq(i) - w(i,j)
          vsti = p(i,j) - u(j)
          call skp1(n,p,w,kqi,vsti,i,kqri,b,a,x,y,u,4,
     &             jdimr,jdimc,jdimc+1,dmyc1,dmyc2,dmyc3,dmycc1,dmycc2,
     &             dmycr1,dmyc4,dmyc5,dmyc6,dmyc7,dmyc8)
          a(i,j) = 0
          y(j) = 1
          delta = vst(i) - vsti
          if ( delta .ge. min2 ) go to 300
          if ( delta .lt. min ) go to 280
          min2 = delta
          if ( min2 .le. 0 ) go to 330
          go to 300
  280     min2 = min
          min = delta
          minz = vsti
          minkq = kqri
          mini = i
          do 290 k=1,n
            miny(k) = y(k)
  290     continue
          if ( min2 .le. 0 ) go to 330
  300   continue
        jstar = j
        istar = mini
        do 320 k=1,n
          if ( miny(k) .eq. 0 ) go to 320
          do 310 i=1,m
            if ( i .eq. istar ) go to 310
            if ( x(i,k) .eq. 1 ) go to 330
  310     continue
  320   continue
        go to 340
  330 continue
      go to 360
c
c step 5.
c
  340 u(jstar) = u(jstar) - min2
      vst(istar) = minz + min2
      kqr(istar) = minkq
      do 350 j=1,n
        x(istar,j) = miny(j)
  350 continue
      jfjvu = jfjvu - min
      go to 130
c
c return.
c
  360 do 370 j=1,n
        if ( b(j) .lt. 0 ) b(j) = 1
  370 continue
      return
      end
      subroutine ubrs(n,m,p,w,q,b,a,jrsu,xrs,vrs,z,inf,
     &                jdimr,jdimc,jdimc1,qh,penrs,pen,u,xk,
     &                dmyc1,dmyc2,dmyc3,dmyc4,dmyc5,dmyc6,dmyc7,
     &                ismax,kpoint,dmycc1,dmycc2,dmycr1)

c*********************************************************************72
c
cc UBRS computes the improved ross-soland upper bound.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer p(jdimr,jdimc),w(jdimr,jdimc),q(jdimr),b(jdimc),
     &        a(jdimr,jdimc),xrs(jdimc),vrs,z
      integer qh(jdimr),penrs(jdimc),pen(jdimc),u(jdimc),xk(jdimc),
     &        ismax(jdimc),kpoint(jdimc)
      integer dmyc1(jdimc),dmyc2(jdimc),dmyc3(jdimc),dmyc4(jdimc),
     &        dmyc5(jdimc),dmyc6(jdimc),dmyc7(jdimc)
      integer dmycc1(jdimc1),dmycc2(jdimc1)
      real    dmycr1(jdimc)
      integer fmax,smax,d
      do i=1,m
        qh(i) = 0
      end do
      jv = 0
      mind = inf
      do 60 j=1,n
        if ( b(j) .eq. 1 ) go to 30
        do 20 i=1,m
          if ( a(i,j) .ne. 1 ) go to 20
          jv = jv + p(i,j)
          xrs(j) = i
          ismax(j) = 0
          go to 60
   20   continue
   30   fmax = - inf
        if = 0
        smax = - 2*inf
        do 50 i=1,m
          if ( a(i,j) .eq. (- 1) ) go to 50
          if ( w(i,j) .gt. q(i) ) go to 50
          if ( smax .ge. p(i,j) ) go to 50
          if ( fmax .ge. p(i,j) ) go to 40
          smax = fmax
          is = if
          fmax = p(i,j)
          if = i
          go to 50
   40     smax = p(i,j)
          is = i
   50   continue
        if ( if .eq. 0 ) go to 160
        xrs(j) = if
        ismax(j) = is
        jv = jv + fmax
        qh(if) = qh(if) + w(if,j)
        penrs(j) = fmax - smax
        if ( fmax - smax .lt. mind ) mind = fmax - smax
   60 continue
      do 70 i=1,m
        if ( qh(i) .gt. q(i) ) go to 80
   70 continue
      jrsu = jv
      vrs = jv
      return
   80 jrsu = jv - mind
      vrs = 0
      if ( jrsu .le. z ) return
c
c compute the minimum penalty for each unsatisfied knapsack.
c
      jrsu = jv
      do 120 i=1,m
        if ( qh(i) .le. q(i) ) go to 120
        d = qh(i) - q(i)
        kk = 0
        mind = inf
        do 90 j=1,n
          if ( b(j) .eq. 0 ) go to 90
          if ( xrs(j) .ne. i ) go to 90
          kk = kk + 1
          pen(kk) = penrs(j)
          u(kk) = w(i,j)
          if ( pen(kk) .lt. mind ) mind = pen(kk)
          kpoint(kk) = j
   90   continue
        if ( jrsu - mind .le. z ) go to 110
        kubf = jrsu - z
        call kpmin(kk,pen,u,d,mind,xk,kubf,
     &             jdimc,jdimc+1,dmycc1,dmycc2,dmyc6,dmyc7,dmycr1,
     &             dmyc1,dmyc2,dmyc3,dmyc4,dmyc5)
        do 100 kj=1,kk
          if ( xk(kj) .eq. 0 ) go to 100
          j = kpoint(kj)
          ismax(j) = - ismax(j)
  100   continue
  110   jrsu = jrsu - mind
        if ( jrsu .le. z ) return
  120 continue
c
c try to obtain a feasible and optimal solution.
c
      do 130 i=1,m
        qh(i) = 0
  130 continue
      do 150 j=1,n
        if ( b(j) .eq. 0 ) go to 150
        if ( ismax(j) .ge. 0 ) go to 140
        xrs(j) = - ismax(j)
  140   i = xrs(j)
        qh(i) = qh(i) + w(i,j)
        if ( qh(i) .gt. q(i) ) return
  150 continue
      vrs = jrsu
      return
c
c infeasibility.
c
  160 jrsu = 0
      vrs = - 1
      return
      end
      subroutine update(n,z,xstar,na,m,x,wb,xred,jdim)

c*********************************************************************72
c
cc UPDATE updates the optimal solution after a local heuristic.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer xstar(jdim),x(jdim),wb(jdim),xred(jdim),z

      do i=1,n
        xstar(i) = x(i)
      end do

      do ii=1,na
        ixrii = xred(ii)
        xstar(ixrii) = wb(ii)
      end do

      z = m

      return
      end
      subroutine upstar(nx,n1,n0,xx,v,loc,x,z,lstar)

c*********************************************************************72
c
cc UPSTAR updates the current optimal solution.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer xx(n0),x(n0),v,z
      z = v
      if ( nx .eq. 0 ) go to 20
      do j=1,nx
        x(j) = xx(j)
      end do

   20 if ( n1 .eq. nx ) go to 40
      nx1 = nx + 1
      do 30 j=nx1,n1
        x(j) = 1
   30 continue
   40 if ( n0 .eq. n1 ) go to 60

      n11 = n1 + 1
      do j=n11,n0
        x(j) = 0
      end do

   60 lstar = loc
      return
      end
      subroutine usedin ( c, td, jdd, nsd, loc )

c*********************************************************************72
c
cc USEDIN finds the maximum vector entry that is no greater than C.
c
c  Discussion:
c
c    This routine determines, through binary search, the location LOC
c    of the entry in TD containing the maximum value TD(LOC,1) <= C.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer c,td(jdd,2)

      if ( td(nsd,1) .gt. c ) go to 10
      loc = nsd
      return
   10 minl = 1
      maxl = nsd
   20 loc = (maxl + minl)/2
      if ( td(loc,1) - c ) 30, 40, 50
   30 minl = loc + 1
      if ( maxl .gt. minl ) go to 20
      loc = minl
      if ( td(loc,1) .le. c ) return
      loc = minl - 1
   40 return
   50 maxl = loc - 1
      if ( maxl .gt. minl ) go to 20
      loc = maxl
      if ( td(loc,1) .le. c ) return
      loc = maxl - 1
      return
      end
      subroutine ydef(l,i,j,ny)

c*********************************************************************72
c
cc YDEF sets y(l,i,j) = NY.
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer       y
      common /pack/ mask1(30),itwo(30),mask,y(150,100)
      iylj = y(l,j)
      imask1 = mask1(i)
      y(l,j) = iand ( iylj, imask1 ) + ny*itwo(i)
      return
      end
      subroutine yuse(l,i,j,ny)

c*********************************************************************72
c
cc YUSE sets ny = y(l,i,j) .
c
c  Modified:
c
c    06 December 2009
c
c  Author:
c
c    Silvano Mortello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    Knapsack Problems: Algorithms and Computer Implementations,
c    Wiley, 1990,
c    ISBN: 0-471-92420-2,
c    LC: QA267.7.M37.
c
      integer       y
      common /pack/ mask1(30),itwo(30),mask,y(150,100)
      iyit = y(l,j)/itwo(i)
      imask = mask

      ny = iand ( iyit, imask )

      return
      end
