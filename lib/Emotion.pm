# Copyright (C) 2000 Free Software Foundation
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

use strict;
package Emotion;
our $VERSION = '0.03';

our $Stem;
our %Link;  # resolve symbolic link
our $DialogID;

sub set_transcript {
    my ($xml) = @_;
    my $stem = $xml;
    $stem =~ s/^.*\///;
    die "$stem is XML?"
	if $stem !~ s/\.xml$//;
    $Stem = $stem;
    %Link = ();
    $DialogID=0;
}

our $Speaker;
our $PhraseID;

sub set_speaker {
    my ($who) = @_;
    $Speaker = $who;
    $PhraseID=0;
    ++$DialogID;
}

our %TypeCode = (destroys => 0,
		 steals => 1,
		 uneasy => 2,
		 exposes => 3,
		 impasse => 4,
		 admires => 5,
		 observes => 6,
		 accepts => 7,
		 ready => 8);

package Emotion::Atom;

sub new {
    my ($class, $expat, $attr) = @_;
    my $o = bless $attr, $class;

    $o->{stem} = $Stem;
    $o->{dialog_id} = $DialogID;
    $o->{phrase_id} = ++$PhraseID;

    my $type = $o->{type};

    if ($type =~ m/^(steals|exposes|admires|accepts)$/) { #spin
	my $before = $o->{before};
	my $after = $o->{after};
	my $tension = $o->{tension};
	if (exists $o->{intensity}) {
	    $expat->xpcarp("rename intensity to tension");
	    $tension = $o->{tension} = delete $o->{intensity};
	}
	$expat->xpcroak("before or after or intensity?")
	    if (defined($before) + defined($after) + defined($tension) != 1);

	my $i = $before || $after || $tension;
	$expat->xpcroak("$i?")
		if $i !~ m/^(focused|relaxed|stifled)$/;
    } elsif ($type eq 'impasse') {
	my $tension = $o->{tension};
	if (exists $o->{intensity}) {
	    $expat->xpcarp("rename intensity to tension");
	    $tension = $o->{tension} = delete $o->{intensity};
	}
	$expat->xpcroak("tension=focused|relaxed|stifled?")
	    if !$tension;
    } elsif ($type =~ m/^(uneasy|observes|ready)$/) {
	my $i = $o->{intensity};
	$expat->xpcroak("intensity=$i? (gentle|forceful|extreme)")
	    if !$i || $i !~ m/^(gentle|forceful|extreme)$/;
    } else { #destroys
	$expat->xpcroak("no intensity needed")
	    if exists $o->{intensity};
    }
	
    my $id = $o->{id};
    if ($id) {
	if (exists $Link{$id}) {
	    my $try = $id;
	    ++$try while exists $Link{$try};
	    $expat->xpcroak("Link collision with `$id' at talk $Link{$id}{dialog_id}, try `$try'");
	}
	$Link{$id} = $o;
    }
	
    if (exists $o->{reply}) {
	$o->{answer} = delete $o->{reply};
	warn "reply renamed to answer";
    }

    for my $kind (qw(context answer)) {
	next if !exists $o->{$kind};
	my $to = $o->{$kind};
	$o->{$kind} = $Link{$to};
	$expat->xpcroak("broken $kind link `$to'")
	    if !$o->{$kind};
    }

    $expat->xpcroak("left and right are missing")
	if !exists $o->{left} && !exists $o->{right};

    for my $role (qw(left right)) {
	next if exists $o->{$role};
	$o->{$role} = $Speaker;
	$o->{initiative} ||= $role
	    if $type !~ m/^(ready|observes|uneasy|impasse|destroys)$/;
    }

    $expat->xpcroak("$o->{left} competing with himself/herself")
	if $o->{left} eq $o->{right};
	
    if ($type =~ m/^(ready|observes|uneasy|impasse|destroys)$/) {
	$expat->xpcroak("initiative is unnecessary")
	    if exists $o->{initiative};
    } else {
	$expat->xpcroak("initiative?")
	    if !exists $o->{initiative};
	my $init = $o->{initiative};
	$expat->xpcroak("left or right?")
	    if $init !~ m/^(left|right)$/;
    }
	
    if (exists $o->{answer}) {
	my $re = $o->{answer};
	$expat->xpcroak("can't answer to readiness")
	    if $re->{type} eq 'ready';
	
	$expat->xpcarp("$o->{left} not in answer")
	    if ($o->{left} ne $re->{left} and $o->{left} ne $re->{right});
	$expat->xpcarp("$o->{right} not in answer")
	    if ($o->{right} ne $re->{left} and $o->{right} ne $re->{right});

	my $i1 = $re->initiator;
	my $i2 = $o->initiator;

	if (!$i1 and !$i2) {
	    $expat->xpcarp("who has initiative?");
	} elsif (defined($i1) xor defined($i2)) {
	    # implied; OK
	} elsif ($i1 eq $i2) {
	    $expat->xpcarp("$i1 kept the initiative")
	}
    }

    $o
}

sub initiator {
    my ($o) = @_;
    if (exists $o->{initiative}) {
	$o->{ $o->{initiative} };
    } elsif ($o->{type} =~ m/^(impasse|destroys)$/) {
	$o->{left};
    } else {
	undef
    }
}

sub simple_hash {
    my ($o) = @_;

    my @k;

    push @k, $TypeCode{ $o->{type} };

    for my $phase (qw(before after tension)) {
	next if !exists $o->{$phase};
	my $pc = do {
	    if ($phase eq 'before') { 0; }
	    elsif ($phase eq 'tension') { 1; }
	    else { 2; }
	};
	push @k, $pc.'='. substr($o->{$phase},0,2);
	last;
    }
    if (exists $o->{intensity}) {
	my $i = $o->{intensity};
	push @k, do {
	    if ($i eq 'gentle') { 1; }
	    elsif ($i eq 'forceful') { 2; }
	    else { 3 }
	};
    }
    
    if ($o->{type} =~ m/^(steals|exposes|admires|accepts)$/) {
	push @k, substr $o->{initiative},0,1;
    }

    join(':', @k)
}

sub hash {
    my ($o) = @_;
    my @a = $o->simple_hash;
    for my $link (qw(answer context)) {
	next if !exists $o->{$link};
	push @a, substr($link,0,2) . ':' . $o->{$link}->simple_hash;
    }
    join ',', @a;
}

sub emotion {
    my ($o) = @_;
    my $ty = $o->{type};
    my $answer;
    my $aty = '';
    if (exists $o->{answer}) {
	$answer = $o->{answer};
	$aty = $answer->{type};
    }
    my $context;
    my $cty = '';
    if (exists $o->{context}) {
	$context = $o->{context};
	$cty = $context->{type};
    }
    if ($ty eq 'exposes') {
	if ($o->{initiative} eq 'left') {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'zealous enthusiasm';
		} elsif ($te eq 'relaxed') {
		    'righteousness';
		} else {
		    'meticulous doubt';
		}
	    } elsif (exists $o->{after}) {
		my $te = $o->{after};
		if ($te eq 'focused') {
		    'vindicated / euphoria';
		} elsif ($te eq 'stifled') {
		    'allow the frenzy to dissipate';
		} else { '?' }
	    } else {
		'?';
	    }
	} else {
	    if ($cty eq 'steals') {
		my $te = $o->{tension};
		if ($te eq 'relaxed') {
		    'researching';
		} else { '?' }
	    } elsif (!$aty or $aty eq 'exposes') {
		my $te = $o->{tension};
		if ($te eq 'focused') {
		    'hatred';
		} elsif ($te eq 'relaxed') {
		    'frustration / sheepishly offer gratitude';
		} else {
		    'embarrassment / denial';
		}
	    } elsif ($aty eq 'accepts' and exists $answer->{before}) {
		my $te = $o->{tension};
		if ($te eq 'focused') {
		    '?';
		} elsif ($te eq 'relaxed') {
		    'admit mistake';
		} else {
		    'try to cover up mistake';
		}
	    } else { '?' }
	}
    } elsif ($ty eq 'steals') {
	if ($o->{initiative} eq 'left') {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'anxiety';
		} elsif ($te eq 'relaxed') {
		    'inwardly uncaring';
		} else {
		    'cool and calculating';
		}
	    } elsif (exists $o->{after}) {
		my $te = $o->{after};
		if ($te eq 'focused') {
		    'drunk with accomplishment';
		} elsif ($te eq 'stifled') {
		    'strain for sobriety';
		} else { '?' }
	    } else {
		'?';
	    }
	} else {
	    my $te = $o->{tension};
	    if (!$aty or $aty eq 'steals') {
		if ($te eq 'focused') {
		    'angry at thief';
		} elsif ($te eq 'stifled') {
		    'angry with his/her self';
		} else { '?' }
	    } elsif ($aty eq 'accepts' and exists $answer->{before}) {
		if ($te eq 'focused') {
		    'accusal';
		} elsif ($te eq 'stifled') {
		    'grudging compliance'
		} else { '?' }
	    } elsif ($aty eq 'exposes') {
		if ($te eq 'focused') {
		    'doubtless righteousness';
		} elsif ($te eq 'relaxed') {
		    'purified desire';
		} else { '?' }
	    } else { '?' }
	}
    } elsif ($ty eq 'admires') {
	if ($o->{initiative} eq 'right') {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'haughty / arrogant';
		} elsif ($te eq 'relaxed') {
		    'humble confidence';
		} else {
		    'seek the means to radiate purity unconditionally'
		}
	    } else {
		my $te = $o->{after};
		if ($te eq 'relaxed') {
		    'reduces the margin for error';
		} else { '?' }
	    }
	} else {
	    my $te = $o->{tension};
	    if (!$aty or $aty eq 'admires') {
		if ($te eq 'focused') {
		    'offer service';
		} elsif ($te eq 'relaxed') {
		    'made whole';
		} else {
		    'whether to become hooked / stuggle to discriminate';
		}
	    } else { '?' }
	}
    } elsif ($ty eq 'accepts') {
	if ($o->{initiative} eq 'right') {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'straightforward demand';
		} elsif ($te eq 'relaxed') {
		    'neutral probe';
		} else {
		    'hoping to be understood';
		}
	    } elsif (exists $o->{after}) {
		my $te = $o->{after};
		if ($te eq 'focused') {
		    'false sense of security';
		} elsif ($te eq 'relaxed') {
		    'relief';
		} else {
		    if (!$aty) {
			'?'
		    } elsif ($aty eq 'accepts' and exists $answer->{before} and
			$answer->{before} eq 'focused') {
			'dubious about a candid assertion';
		    } elsif ($aty eq 'observes' and
			     $answer->{intensity} eq 'gentle') {
			'dubious about a brush-off reply';
		    } else {
			'dubious';
		    }
		}
	    } else {
		'?';
	    }
	} else {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    if ($aty eq 'impasse' and 
			$answer->{tension} eq 'relaxed') {
			'guess the riddle';
		    } else {
			'confidence';
		    }
		} elsif ($te eq 'relaxed') {
		    'probe of agreement'
		} else { '?' }
	    } elsif (exists $o->{after}) {
		my $te = $o->{after};
		if ($aty eq 'accepts') {
		    if ($te eq 'focused') {
			'balloon enthusiasm out of control';
		    } elsif ($te eq 'stifled') {
			'dissipate delight';
		    } else { '?' }
		} else { '?' }
	    } else {
		my $te = $o->{tension};
		if (!$aty) {
		    if ($te eq 'focused') {
			'it is done';
		    } else {
			'?';
		    }
		} elsif ($aty eq 'accepts') {
		    if ($te eq 'focused') {
			'seems to accept';
		    } elsif ($te eq 'relaxed') {
			'embraces wish';
		    } else { '?' }
		} elsif ($aty eq 'impasse') {
		    if ($te eq 'focused') {
			'cheer impasse';
		    } elsif ($te eq 'relaxed') {
			'acknowledge impasse';
		    } else {
			'concede impasse';
		    }
		} elsif ($aty eq 'steals') {
		    if ($te eq 'focused') {
			'convert to admiration';
		    } elsif ($te eq 'relaxed') {
			'force humility';
		    } else {
			'convert to uneasy';
		    }
		} elsif ($aty eq 'exposes') {
		    if ($te eq 'focused') {
			'whoops!';
		    } elsif ($te eq 'relaxed') {
			'oh, ah / silence';
		    } else {
			'please let me try to explain';
		    }
		} else { '?' }
	    }
	}
    } elsif ($ty eq 'destroys') {
	'death';
    } elsif ($ty eq 'uneasy') {
	if (!$aty) {
	    my $in = $o->{intensity};
	    if ($in eq 'gentle') {
		'nervousness';
	    } elsif ($in eq 'forceful') {
		'discomfort';
	    } else {
		'sleepless agony';
	    }
	} elsif ($aty eq 'accepts' and exists $answer->{before} and
		 $answer->{before} eq 'focused' and
		 $answer->{initiative} eq 'right') {
	    my $in = $o->{intensity};
	    if ($in eq 'gentle') {
		'caught red-handed';
	    } else { '?' }
	} elsif ($aty eq 'accepts' and exists $answer->{before} and
		 $answer->{before} eq 'focused' and
		 $answer->{initiative} eq 'left') {
	    my $in = $o->{intensity};
	    if ($in eq 'gentle') {
		'suspicious';
	    } elsif ($in eq 'forceful') {
		'react to fear by striving for harmony';
	    } else {
		'faint under pressure';
	    }
	} else { '?' }
    } elsif ($ty eq 'observes') {
	my $in = $o->{intensity};
	if (!$aty) {
	    if ($in eq 'gentle') {
		'excitement';
	    } elsif ($in eq 'forceful') {
		'exciting test of self-control';
	    } else {
		'challenge of emotional insight';
	    }
	} elsif ($aty eq 'accepts') {
	    if ($in eq 'gentle') {
		'seems to accept';
	    } elsif ($in eq 'forceful') {
		'(silence)';
	    } else {
		'blasts her with silent force'
	    }
	} else { '?' }
    } elsif ($ty eq 'ready') {
	my $in = $o->{intensity};
	if ($in eq 'gentle') {
	    'emotional turmoil';
	} elsif ($in eq 'forceful') {
	    'limbo';
	} else {
	    'Ready!';
	}
    } elsif ($ty eq 'impasse') {
	my $te = $o->{tension};
	if ($te eq 'focused') {
	    'stubborn / frustrated / indignant';
	} elsif ($te eq 'relaxed') {
	    'probe / sincere and balanced concern';
	} else {
	    'separation';
	}
    } else { '?' }
}

1;
