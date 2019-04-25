text = $stdin.read.chars

100.times do
    text.delete_at(rand(text.size()))
end

puts text.join