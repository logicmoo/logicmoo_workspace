/*  Part of SWI-Prolog

    Author:        Matt Lilley
    E-mail:        matt.s.lilley@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, Mike Elston, Matt Lilley
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*  PostgreSQL is a trademark of the PostgreSQL Global Development Group.
    Microsoft, SQL Server, and Windows are either registered trademarks or
    trademarks of Microsoft Corporation in the United States and/or other
    countries. SQLite is a registered trademark of Hipp, Wyrick & Company,
    Inc in the United States. All other trademarks or registered trademarks
    are the property of their respective owners.
*/

:-module(sql_keywords,
         [reserved_sql_keyword/1]).

reserved_sql_keyword(absolute).
reserved_sql_keyword(add).
reserved_sql_keyword(all).
reserved_sql_keyword(allocate).
reserved_sql_keyword(alter).
reserved_sql_keyword(and).
reserved_sql_keyword(any).
reserved_sql_keyword(are).
reserved_sql_keyword(as).
reserved_sql_keyword(asc).
reserved_sql_keyword(assertion).
reserved_sql_keyword(authorization).
reserved_sql_keyword(avg).
reserved_sql_keyword(begin).
reserved_sql_keyword(between).
reserved_sql_keyword(bit).
reserved_sql_keyword(bit_length).
reserved_sql_keyword(both).
reserved_sql_keyword(by).
reserved_sql_keyword(cascade).
reserved_sql_keyword(cascaded).
reserved_sql_keyword(case).
reserved_sql_keyword(cast).
reserved_sql_keyword(catalog).
reserved_sql_keyword(char).
reserved_sql_keyword(character).
reserved_sql_keyword(character_length).
reserved_sql_keyword(char_length).
reserved_sql_keyword(check).
reserved_sql_keyword(close).
reserved_sql_keyword(coalesce).
reserved_sql_keyword(collate).
reserved_sql_keyword(collation).
reserved_sql_keyword(column).
reserved_sql_keyword(commit).
reserved_sql_keyword(connect).
reserved_sql_keyword(connection).
reserved_sql_keyword(constraint).
reserved_sql_keyword(constraints).
reserved_sql_keyword(continue).
reserved_sql_keyword(convert).
reserved_sql_keyword(corresponding).
reserved_sql_keyword(create).
reserved_sql_keyword(cross).
reserved_sql_keyword(current).
reserved_sql_keyword(current_date).
reserved_sql_keyword(current_time).
reserved_sql_keyword(current_timestamp).
reserved_sql_keyword(current_user).
reserved_sql_keyword(cursor).
%reserved_sql_keyword(date).
reserved_sql_keyword(day).
reserved_sql_keyword(deallocate).
reserved_sql_keyword(dec).
reserved_sql_keyword(decimal).
reserved_sql_keyword(declare).
reserved_sql_keyword(default).
reserved_sql_keyword(deferrable).
reserved_sql_keyword(deferred).
reserved_sql_keyword(delete).
reserved_sql_keyword(desc).
reserved_sql_keyword(describe).
reserved_sql_keyword(descriptor).
reserved_sql_keyword(diagnostics).
reserved_sql_keyword(disconnect).
reserved_sql_keyword(distinct).
reserved_sql_keyword(domain).
reserved_sql_keyword(double).
reserved_sql_keyword(drop).
reserved_sql_keyword(else).
reserved_sql_keyword(end).
reserved_sql_keyword(exec).
reserved_sql_keyword(escape).
reserved_sql_keyword(except).
reserved_sql_keyword(execute).
reserved_sql_keyword(exists).
reserved_sql_keyword(external).
reserved_sql_keyword(extract).
reserved_sql_keyword(false).
reserved_sql_keyword(fetch).
reserved_sql_keyword(first).
reserved_sql_keyword(for).
reserved_sql_keyword(foreign).
reserved_sql_keyword(found).
reserved_sql_keyword(from).
reserved_sql_keyword(full).
reserved_sql_keyword(get).
reserved_sql_keyword(global).
reserved_sql_keyword(go).
reserved_sql_keyword(goto).
reserved_sql_keyword(grant).
reserved_sql_keyword(group).
reserved_sql_keyword(having).
reserved_sql_keyword(hour).
reserved_sql_keyword(identity).
reserved_sql_keyword(immediate).
reserved_sql_keyword(in).
reserved_sql_keyword(indicator).
reserved_sql_keyword(initially).
reserved_sql_keyword(inner).
reserved_sql_keyword(input).
reserved_sql_keyword(insensitive).
reserved_sql_keyword(insert).
reserved_sql_keyword(int).
reserved_sql_keyword(integer).
reserved_sql_keyword(intersect).
reserved_sql_keyword(interval).
reserved_sql_keyword(into).
reserved_sql_keyword(is).
reserved_sql_keyword(isolation).
reserved_sql_keyword(join).
reserved_sql_keyword(key).
reserved_sql_keyword(language).
reserved_sql_keyword(last).
reserved_sql_keyword(leading).
reserved_sql_keyword(left).
reserved_sql_keyword(level).
reserved_sql_keyword(like).
reserved_sql_keyword(local).
reserved_sql_keyword(lower).
reserved_sql_keyword(match).
reserved_sql_keyword(max).
reserved_sql_keyword(min).
reserved_sql_keyword(minute).
reserved_sql_keyword(module).
reserved_sql_keyword(names).
reserved_sql_keyword(national).
reserved_sql_keyword(natural).
reserved_sql_keyword(nchar).
reserved_sql_keyword(next).
reserved_sql_keyword(no).
reserved_sql_keyword(not).
reserved_sql_keyword(null).
reserved_sql_keyword(nullif).
reserved_sql_keyword(numeric).
reserved_sql_keyword(octet_length).
reserved_sql_keyword(of).
reserved_sql_keyword(on).
reserved_sql_keyword(only).
reserved_sql_keyword(open).
reserved_sql_keyword(option).
reserved_sql_keyword(or).
reserved_sql_keyword(order).
reserved_sql_keyword(outer).
reserved_sql_keyword(output).
reserved_sql_keyword(overlaps).
reserved_sql_keyword(pad).
reserved_sql_keyword(partial).
reserved_sql_keyword(position).
reserved_sql_keyword(precision).
reserved_sql_keyword(prepare).
reserved_sql_keyword(preserve).
reserved_sql_keyword(primary).
reserved_sql_keyword(prior).
reserved_sql_keyword(privileges).
reserved_sql_keyword(procedure).
reserved_sql_keyword(public).
reserved_sql_keyword(read).
reserved_sql_keyword(real).
reserved_sql_keyword(references).
reserved_sql_keyword(relative).
reserved_sql_keyword(restrict).
reserved_sql_keyword(revoke).
reserved_sql_keyword(right).
reserved_sql_keyword(rollback).
reserved_sql_keyword(schema).
reserved_sql_keyword(scroll).
reserved_sql_keyword(second).
reserved_sql_keyword(section).
reserved_sql_keyword(select).
reserved_sql_keyword(session).
reserved_sql_keyword(session_user).
reserved_sql_keyword(set).
reserved_sql_keyword(size).
reserved_sql_keyword(smallint).
reserved_sql_keyword(some).
reserved_sql_keyword(space).
reserved_sql_keyword(sql).
reserved_sql_keyword(sqlcode).
reserved_sql_keyword(sqlerror).
reserved_sql_keyword(sqlstate).
reserved_sql_keyword(substring).
reserved_sql_keyword(sum).
reserved_sql_keyword(system_user).
reserved_sql_keyword(table).
reserved_sql_keyword(temporary).
reserved_sql_keyword(then).
reserved_sql_keyword(time).
reserved_sql_keyword(timestamp).
reserved_sql_keyword(timezone_hour).
reserved_sql_keyword(timezone_minute).
reserved_sql_keyword(to).
reserved_sql_keyword(trailing).
reserved_sql_keyword(transaction).
reserved_sql_keyword(translate).
reserved_sql_keyword(translation).
reserved_sql_keyword(trim).
reserved_sql_keyword(true).
reserved_sql_keyword(union).
reserved_sql_keyword(unique).
reserved_sql_keyword(unknown).
reserved_sql_keyword(update).
reserved_sql_keyword(upper).
reserved_sql_keyword(usage).
reserved_sql_keyword(user).
reserved_sql_keyword(using).
reserved_sql_keyword(values).
reserved_sql_keyword(varchar).
reserved_sql_keyword(varying).
reserved_sql_keyword(view).
reserved_sql_keyword(when).
reserved_sql_keyword(whenever).
reserved_sql_keyword(where).
reserved_sql_keyword(with).
reserved_sql_keyword(work).
reserved_sql_keyword(write).
reserved_sql_keyword(zone).


reserved_sql_keyword(isnull).
reserved_sql_keyword(ltrim).
reserved_sql_keyword(replace).
reserved_sql_keyword(dateadd).
reserved_sql_keyword(datediff).
reserved_sql_keyword(charindex).
reserved_sql_keyword(datepart).
reserved_sql_keyword(abs).
