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

weighted_sum = 0.0
sum = 0.0

graph_data.each do |k, v|
    if k >= 0 then
        weighted_sum += (k*v)
        sum += v
    end
end

puts weighted_sum/sum
