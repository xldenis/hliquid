require 'liquid'

Liquid::Template.error_mode = :strict
@template = Liquid::Template.parse(
<<-TEST
<table>
  {% tablerow product in collection.products cols: /// %}
    {{ product.title }}
  {% endtablerow %}
</table>
TEST
)
puts "AAAA"
puts @template.warnings
puts @template.errors
puts @template.inspect
