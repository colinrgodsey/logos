package com.colingodsey.logos.collections

import scala.collection.immutable

/*Rethink of sets

Map is a Coll with the unique constraint as x => id.
Set is a Coll with the unique constraint x => x.
Seq is a Coll with external unique constraint (record ID).

Coll can have up to 1 unique constraint, and several indexes.
Unique index has up to 1 value for a key, non-unique has up to n.

 */


