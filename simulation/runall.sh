dos2unix ~/VT/Chuyu/Scripts/run_vt_sim.sh


#### linear covariate effects, no TEH
sbatch --export=c_name='J101',dg=lin_null_dg,vt1=i.las,sims=10 --job-name=J10.las -t 10:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J102',dg=lin_null_dg,vt1=i.rf.fast,sims=10 --job-name=J10.rf -t 20:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J103',dg=lin_null_dg,vt1=i.mars,sims=10 --job-name=J10.mars -t 10:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J104',dg=lin_null_dg,vt1=i.super.fast,sims=10 --job-name=J10.super -t 24:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### linear covariate effects, linear TEH
sbatch --export=c_name='J111',dg=lin_lin_dg,vt1=i.las,sims=10 --job-name=J11.las -t 10:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J112',dg=lin_lin_dg,vt1=i.rf.fast,sims=10 --job-name=J11.rf -t 20:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J113',dg=lin_lin_dg,vt1=i.mars,sims=10 --job-name=J11.mars -t 10:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J114',dg=lin_lin_dg,vt1=i.super.fast,sims=10 --job-name=J11.super -t 24:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### linear covariate effects, nonlinear TEH
sbatch --export=c_name='J121',dg=lin_nonlin_dg,vt1=i.las,sims=10 --job-name=J12.las -t 15:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J122',dg=lin_nonlin_dg,vt1=i.rf.fast,sims=10 --job-name=J12.rf -t 24:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J123',dg=lin_nonlin_dg,vt1=i.mars,sims=10 --job-name=J12.mars -t 15:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J124',dg=lin_nonlin_dg,vt1=i.super.fast,sims=10 --job-name=J12.super -t 24:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### nonlinear covariate effects, no TEH
sbatch --export=c_name='J201',dg=nonlin_null_dg,vt1=i.las,sims=10 --job-name=J20.las -t 10:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J202',dg=nonlin_null_dg,vt1=i.rf.fast,sims=10 --job-name=J20.rf -t 20:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J203',dg=nonlin_null_dg,vt1=i.mars,sims=10 --job-name=J20.mars -t 10:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J204',dg=nonlin_null_dg,vt1=i.super.fast,sims=10 --job-name=J20.super -t 24:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### nonlinear covariate effects, linear TEH
sbatch --export=c_name='J211',dg=nonlin_lin_dg,vt1=i.las,sims=10 --job-name=J21.las -t 10:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J212',dg=nonlin_lin_dg,vt1=i.rf.fast,sims=10 --job-name=J21.rf -t 20:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J213',dg=nonlin_lin_dg,vt1=i.mars,sims=10 --job-name=J21.mars -t 10:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J214',dg=nonlin_lin_dg,vt1=i.super.fast,sims=10 --job-name=J21.super -t 24:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### nonlinear covariate effects, nonlinear TEH
sbatch --export=c_name='J221',dg=nonlin_nonlin_dg,vt1=i.las,sims=10 --job-name=J22.las -t 15:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J222',dg=nonlin_nonlin_dg,vt1=i.rf.fast,sims=10 --job-name=J22.rf -t 24:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J223',dg=nonlin_nonlin_dg,vt1=i.mars,sims=10 --job-name=J22.mars -t 15:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='J224',dg=nonlin_nonlin_dg,vt1=i.super.fast,sims=10 --job-name=J22.super -t 24:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

###############################################################################

