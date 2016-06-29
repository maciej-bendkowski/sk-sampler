#!/usr/bin/ruby
graph_data = Hash.new 0
ARGF.each_line do |line|
    rs = line.to_i
    if graph_data[rs] == 0 then
        graph_data[rs] = 1
    else
        graph_data[rs] = graph_data[rs] + 1
    end
end

graph_data.sort.each do |key, value|
    puts "#{key},#{value}"
end
