#!ruby
#
# Reads a list of photo urls and IDs from a CSV file or URL, then uses the
# Google vision API (part of the Google Cloud Platform) to create a
# list of content labels with probabilities for each photo.
#
# Usage : photo_content.rb [--id <id col>] [--url <url col>] [--append out.csv] <photos.csv>


require 'csv'
require 'open-uri'
require 'optparse'
require 'set'

################################################################################
### Google vision stuff

require "google/cloud/vision"

def image_labels(image_annotator, url)
  # Performs label detection on the image file
  labels = {}
  response = image_annotator.label_detection image: url
  response.responses.each do |res|
    res.label_annotations.each do |label|
      labels[label.description] = label.score
    end
  end
  labels
end


################################################################################
### Command line processing

options = {inID: 'id', url: 'url', base: ''}
OptionParser.new do |opt|
  opt.on('--project PROJECT', 'Google project id') { |o| options[:project] = o }
  opt.on('--keyfile FILE', 'Google JSON key file') { |o| options[:keyfile] = o }
  opt.on('--inID COLUMN', ' Name of input ID column') { |o| options[:inID] = o }
  opt.on('--url COLUMN', 'Name of URL column') { |o| options[:url] = o }
  opt.on('--base PATH', 'Prefix for URL') { |o| options[:base] = o }
  opt.on('--append FILE', 'Append output to FILE') { |o| options[:append] = o }
end.parse!
in_filename = ARGV.pop
raise "Missing input file name" unless in_filename

################################################################################

# Work out which photos have already been processed (it costs real money, so do as little as possible)
ids_to_skip = Set.new
if options[:append] && File.file?(options[:append])
  CSV.foreach(options[:append], headers: true) do |row|
    ids_to_skip.add(row['id'])
  end
end

#puts "Skipping"
#p ids_to_skip
#exit

###
# Authorization credentials are taken from a json file referenced by
# $GOOGLE_APPLICATION_CREDENTIALS
image_annotator = Google::Cloud::Vision::ImageAnnotator.new

# Output CSV
out_io = options[:append] ? File.open(options[:append], 'a') : $stdout.dup
out_csv = CSV.new(out_io)
out_csv << ['id', 'label', 'probability'] if ids_to_skip.empty?

### Read through photo CSV...
CSV.foreach(open(in_filename), {:headers => true}) do |row|
  p_id = row[options[:inID]]
  if ids_to_skip.include? p_id
    puts "Skipping #{p_id}"
  else
    url = options[:base] + row[options[:url]]
    puts "Querying #{p_id}: #{url}"
    image_labels(image_annotator, url).each { |k, v| out_csv << [p_id, k, v] }
  end
end

out_csv.close
