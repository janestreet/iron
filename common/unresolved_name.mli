(** Some users have alternate names for convenience, for example when their regular
    username is too long for example, or for various reasons.  For the sake of the
    example, let's consider the user 'John Actuallydoe'.  (So their last name is Doe?  No,
    it's Actuallydoe.  Well it's Doe, right?  No, no it's Actuallydoe).  It was decided
    that, for convenience, the user may be referred to using the shorter unresolved name
    [jadoe] rather than their full username [jactuallydoe].

    Another form of alternate name is a common typo.  Some user names are unfortunately
    very often typed with a typo, and sometimes it seemed just easier to have the system
    be made aware of that fact.  Example: if too many people make the mistake of referring
    to [John Misple] as [jmisspelled] instead of [jmisple], we'll maybe just decide that
    at some point, [jmisspelled] might just be used as a proxy for [jmisple].

    A value of type [Unresolved_name.t] can be either an alternate name that can be a
    alias (e.g. [jadoe]) or a typo (e.g. [jmisspelled]), the user name of a user that have
    an alternate name (e.g. [jactuallydoe]), or just the user name of a user who does not
    have an alternate name (e.g. [jsmith]).

    This set equation holds:

    {v
     Unresolved name = Alternate name + User name
    v}

    Unresolved names are used for inputs where alternate names are permitted (crs,
    obligations) and these can be resolved into user_names by
    [User_name_by_alternate_name]. *)

open! Core
open! Import

include Validated_string.S
