################################################################################
#                                                                              #
#    VARIABLES A MODIFICAR PARA CADA DESPLIEGUE                                #
#                                                                              #
################################################################################

NODES = [
  { hostname: 'pilgor', ip: 'pilgor.cps.unizar.es', cpus: 90 },
  { hostname: 'berlin', ip: 'berlin.unizar.es', cpus: 30 },

]

################################################################################
#                                                                              #
#    VARIABLES AUTOGENERADAS                                                   #
#                                                                              #
################################################################################

# TOTAL_CPUS = NODES.sum { |node| node[:cpus].to_i }
#no se si falla xd
TOTAL_CPUS = 120

################################################################################
#                                                                              #
#    CORE DEL PROGRAMA                                                         #
#                                                                              #
################################################################################

currentcpus = 0
NODES.each do |node|
    ssh_command = "ssh  -n #{node[:hostname]} "
    system(ssh_command + "'" +"cp grafica /tmp" + "'")
    spawn(ssh_command + "'" + "cd /tmp/grafica/tmp; ./run_param.sh #{currentcpus} #{currentcpus + node[:cpus]} #{TOTAL_CPUS} #{node[:hostname]} ; cd /tmp; rm -rf ./grafica" + "'")
    currentcpus += node[:cpus]
end
