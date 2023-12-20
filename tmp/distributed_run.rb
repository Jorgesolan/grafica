require 'optparse'

shutdown = false

OptionParser.new do |parser|
  parser.on("-shutdown", "Execute shutdown") do
    shutdown = true
  end
end.parse!
################################################################################
#                                                                              #
#    VARIABLES A MODIFICAR PARA CADA DESPLIEGUE                                #
#                                                                              #
################################################################################

NODES = [
  { hostname: 'pilgor', ip: 'pilgor.cps.unizar.es', cpus: 90, binExt: "pilgor" },
  { hostname: 'berlin', ip: 'berlin.unizar.es', cpus: 30, binExt: "berlin" },
  # { hostname: 'lab102-191', ip: '155.210.154.191', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-192', ip: '155.210.154.192', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-193', ip: '155.210.154.193', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-194', ip: '155.210.154.194', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-195', ip: '155.210.154.195', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-196', ip: '155.210.154.196', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-197', ip: '155.210.154.197', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-198', ip: '155.210.154.198', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-199', ip: '155.210.154.199', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-200', ip: '155.210.154.200', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-201', ip: '155.210.154.201', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-202', ip: '155.210.154.202', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-203', ip: '155.210.154.203', cpus: 6, binExt: "lab102" },
  { hostname: 'lab102-204', ip: '155.210.154.204', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-205', ip: '155.210.154.205', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-206', ip: '155.210.154.206', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-207', ip: '155.210.154.207', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-208', ip: '155.210.154.208', cpus: 6, binExt: "lab102" }, ### este si funciona pero se tiene q borrar el /tmp antes
  # { hostname: 'lab102-209', ip: '155.210.154.209', cpus: 6, binExt: "lab102" },
  # { hostname: 'lab102-210', ip: '155.210.154.210', cpus: 6, binExt: "lab102" },
]

################################################################################
#                                                                              #
#    VARIABLES AUTOGENERADAS                                                   #
#                                                                              #
################################################################################
TOTAL_CPUS = 174
# TOTAL_CPUS = NODES.sum { |node| node[:cpus].to_i }
LAB102NODES = NODES.select { |node| node[:hostname].include?('lab102') }

################################################################################
#                                                                              #
#    CORE DEL PROGRAMA                                                         #
#                                                                              #
################################################################################
if !shutdown
  currentcpus = 0
  NODES.each do |node|
      ssh_command = "ssh  -n #{node[:hostname]} "
      ping_successful = system("ping -c 1 #{node[:hostname]}")
      if ping_successful
        system(ssh_command + "'" +"cp -r grafica /tmp" + "'")
        spawn(ssh_command + "'" + "cd /tmp/grafica/tmp; ./run_param.sh #{currentcpus} #{currentcpus + node[:cpus]} #{TOTAL_CPUS} #{node[:binExt]} " + "'")
        currentcpus += node[:cpus]
      else
        puts "#{node} is fallen"
      end
  end
  puts "CPUs empleadas #{currentcpus}"
else
  NODES.each do |node|
    ssh_command = "ssh  -n #{node[:hostname]} "
    ping_successful = system("ping -c 1 #{node[:hostname]}")
    if ping_successful
      system(ssh_command + "'" +"rm -rf /tmp/grafica" + "'")
    else
      puts "#{node} is fallen"
    end
  end
  system("./apagar.sh")
end