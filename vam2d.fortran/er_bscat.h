c!#   $Id: er_bscat.h,v 1.1 1997/02/10 16:39:08 leidner Exp $
c!#   $Log: er_bscat.h,v $
c!#   Revision 1.1  1997/02/10 16:39:08  leidner
c!#   Initial revision (filename originally er_bscat.com)
c!#
      integer er_nobs,er_ant(ER_MOBS,ER_MBEAM)
      integer er_ic(ER_MOBS),er_jc(ER_MOBS)
      integer er_qc(ER_MOBS),er_tag(ER_MOBS)

      real er_lat(ER_MOBS),er_lon(ER_MOBS),er_theta(ER_MOBS,ER_MBEAM)
      real er_azim(ER_MOBS,ER_MBEAM),er_S0obs(ER_MOBS,ER_MBEAM)
      real er_kp(ER_MOBS,ER_MBEAM),er_s0sd(ER_MOBS,ER_MBEAM)
      real er_velobs(ER_MOBS),er_dirobs(ER_MOBS),er_time(ER_MOBS)
      real er_s05(ER_MOBS,ER_MBEAM),er_vel5(ER_MOBS),er_dir5(ER_MOBS)
      real er_s0bg(ER_MOBS,ER_MBEAM),er_velbg(ER_MOBS),er_dirbg(ER_MOBS)
      real er_xc(ER_MOBS),er_yc(ER_MOBS)

      common /ER_BSCAT/ er_lat,er_lon,er_theta,er_azim,er_s0obs,er_kp,
     &                  er_s0sd,er_velobs,er_dirobs,er_time,er_xc,
     &                  er_yc,er_s0bg,er_velbg,er_dirbg,er_s05,er_vel5,
     &                  er_dir5,er_nobs,er_ant,er_ic,er_jc,er_qc,er_tag
