#!/bin/bash
set -euxo pipefail
# ---------------------------- stop_fds_if_requested ----------------------------------

# function stop_fds_if_requested {
# if [ "$STOPFDS" != "" ]; then
#   echo "stopping case: $in"
#  touch $stopfile $stopcatfile
#  exit
# fi

# if [ "$STOPFDSMAXITER" != "" ]; then
#   echo "creating delayed stop file: $infile"
#   echo $STOPFDSMAXITER > $stopfile
#   echo $STOPFDSMAXITER > $stopcatfile
# fi

# if [ "$stopjob" == "1" ]; then
#   echo "stopping case: $in"
#   touch $stopfile $stopcatfile
#   exit
# fi

# if [ "$STOPFDSMAXITER" == "" ]; then
#   rm -f $stopfile $stopcatfile
# fi
# }

dir=""
exe="fds"
queue="debug"
n_openmp_threads=1
n_mpi_processes=1
JOBPREFIX=""
showinput=0
nodelist=
# ---------------------------- usage ----------------------------------

function usage {
  echo "Usage: qfds.sh [-p n_mpi_processes] [-o nthreads] [-e fds_command] [-q queue]  casename.fds"
  echo ""
  echo "qfds.sh runs FDS using an executable from the repository or one specified with the -e option."
  echo ""
  echo " -e exe - full path of FDS used to run case "
  echo "    [default: fds]"
  echo " -h   - show commonly used options"
  echo " -H   - show all options"
  echo " -o o - number of OpenMP threads per process [default: 1]"
  echo " -p p - number of MPI processes [default: 1] "
  echo " -q q - name of queue. [default: batch]"
  echo " -v   - output generated script to standard output"
  if [ "$HELP" == "" ]; then
    return
  fi
  echo "Other options:"
  echo " -d dir - specify directory where the case is found [default: .]"
  echo " -j prefix - specify a job prefix"
  echo " -n n - number of MPI processes per node [default: 1]"
}

while getopts 'b:d:e:GhHIj:Ln:o:Pp:q:stT:U:vw:y:Y' OPTION
do
case $OPTION  in
  d)
   dir="$OPTARG"
   ;;
  e)
   exe="$OPTARG"
   ;;
  h)
   usage
   exit
   ;;
  H)
   HELP=ALL
   usage
   exit
   ;;
  j)
   JOBPREFIX="$OPTARG"
   ;;
  w)
   nodelist="$OPTARG"
   ;;
  o)
   n_openmp_threads="$OPTARG"
   ;;
  p)
   n_mpi_processes="$OPTARG"
   ;;
  q)
   queue="$OPTARG"
   ;;
  v)
   echo "show input"
   showinput=1
   ;;
  *)
   usage
   exit 1
   ;;
esac
done
shift $(($OPTIND-1))

#*** define input file
in=$1
infile=${in%.*}
if [ "$dir" == "" ]; then
  in=$(realpath "$1")
else
  in=$(realpath "$dir"/"$1")
fi
dir=$(dirname "$in")

#*** define number of nodes

nodes=1 # $(((n_mpi_processes+n_mpi_processes_per_node-1)/n_mpi_processes_per_node))

echo "n_openmp_threads: $n_openmp_threads"
echo "n_mpi_processes: $n_mpi_processes"

# cd $dir
fulldir=$dir

#*** define files

outerr=$fulldir/$infile.err
outlog=$fulldir/$infile.log
qlog=$fulldir/$infile.qlog
scriptlog=$fulldir/$infile.slog
# in_full_file=$fulldir/$in

echo "$exe $in"

# Print useful job information
echo "========================================="
echo "outerr=$fulldir/$infile.err"
echo "outlog=$fulldir/$infile.log"
echo "qlog=$fulldir/$infile.qlog"
echo "scriptlog=$fulldir/$infile.slog"
echo "in_full_file=$fulldir/$in"
echo "========================================="

#*** create a random script filename for submitting jobs

scriptfile=$(mktemp /tmp/script.$$.XXXXXX)

cat << EOF > "$scriptfile"
#!/bin/bash
EOF

cat << EOF >> "$scriptfile"
#SBATCH -J $JOBPREFIX$infile
#SBATCH -e $outerr
#SBATCH -o $outlog
#SBATCH --partition=$queue
#SBATCH --ntasks=$n_mpi_processes
#SBATCH --cpus-per-task=$n_openmp_threads
#SBATCH --nodes=$nodes
EOF


cat << EOF >> "$scriptfile"

echo "========================================="
echo "Job ID: \$SLURM_JOBID"
echo "Job Name: \$SLURM_JOB_NAME"
echo "Nodes: \$SLURM_JOB_NODELIST"
echo "CPUs per task: \$SLURM_CPUS_PER_TASK"
echo "Tasks per node: \$SLURM_NTASKS_PER_NODE"
echo "Host: \$(hostname)"
echo "Working directory: \$(pwd)"
echo "Start time: \$(date)"
echo "========================================="
EOF

# if [ "$benchmark" == "yes" ]; then
# cat << EOF >> $scriptfile
# #SBATCH --exclusive
# #SBATCH --cpu-freq=Performance
# EOF
# fi

cat << EOF >> "$scriptfile"
export OMP_NUM_THREADS=$n_openmp_threads
EOF

# if [[ $n_openmp_threads -gt 1 ]] && [[ "$use_intel_mpi" == "1" ]]; then
# cat << EOF >> "$scriptfile"
# export I_MPI_PIN_DOMAIN=omp
# EOF
# fi

cat << EOF >> "$scriptfile"

cd $fulldir
echo
echo \`date\`
EOF

cat << EOF >> "$scriptfile"
echo "    Input file: $in"
EOF

cat << EOF >> "$scriptfile"
. /mnt/data-drive/infra/spack/share/spack/setup-env.sh
spack env activate -d /mnt/data-drive/infra/fds-env/v1/6.10.1
spack load
ulimit -s unlimited
srun --mpi=pmi2 $exe $in
EOF

cat << EOF >> "$scriptfile"
echo "finished running at \$(date)" >> $qlog
EOF

#*** output script file to screen if -v option was selected

if [ "$showinput" == "1" ]; then
  cat "$scriptfile"
  echo
  exit
fi

#*** output info to screen

echo "submitted at $(date)"
echo "         Input file:$in"
echo "         Executable:$exe"

#*** output modules used when fds is run

CURRENT_LOADED_MODULES=$(echo "$LOADEDMODULES" | tr ':' ' ')
MODULES=$CURRENT_LOADED_MODULES

if [ "$MODULES" != "" ]; then
    echo "            Modules:$MODULES"
fi
# echo "   Resource Manager:$RESOURCE_MANAGER"
echo "              Queue:$queue"
echo "              Nodes:$nodes"
echo "          Processes:$n_mpi_processes"
if  [[ ! -z "$n_openmp_threads" ]] && [ "$n_openmp_threads" -gt 1 ] ; then
  echo "Threads per process:$n_openmp_threads"
fi

#*** run script

chmod +x "$scriptfile"

nodelistarg=""
if [[ ! -z "$nodelist" ]]; then
  nodelistarg="-w $nodelist"
fi

sbatch -p "$queue" $nodelistarg "$scriptfile" | tee -a "$qlog"

cat "$scriptfile" > "$scriptlog"
rm "$scriptfile"
exit
