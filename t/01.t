#!./perl -w

use strict;
use Carp;
use Test; plan tests => 2;
use XML::Parser;
use Emotion;

Emotion::set_transcript('test.xml');
Emotion::set_speaker('Test');
my $p = XML::Parser->new(ErrorContext => 1);
$p->setHandlers(Start => \&start);
$p->parse(join('', <DATA>));

sub start {
    my ($expat, $elem, %attr) = @_;
    return if $elem eq 'all';
    $attr{type} = $elem;
    Emotion::Atom->new($expat, \%attr);
}

my @pend = Emotion::unresolved();
ok 0+@pend, 1;
ok $pend[0]->{type}, 'impasse';

__DATA__
<all>

  <accepts id="order1" initiator="clerk" left="Joey" before="focused">
    What toppings do you want on your pizza?
  </accepts>

  <impasse re="order1" left="Joey" right="clerk" tension="stifled">
    Joey puts the phone on hold.
  </impasse>

</all>
