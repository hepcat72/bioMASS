#!/usr/bin/perl -w

#extractPeaks.pl
#Generated using perl_script_template.pl 1.31
#Robert W. Leach
#rwleach@ccr.buffalo.edu
#Created on 8/14/2008
#Center for Computational Research
#Provided by GNU GPL

#                    GNU GENERAL PUBLIC LICENSE
#                       Version 3, 29 June 2007
#
# Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
# Everyone is permitted to copy and distribute verbatim copies
# of this license document, but changing it is not allowed.
#
#                            Preamble
#
#  The GNU General Public License is a free, copyleft license for
#software and other kinds of works.
#
#  The licenses for most software and other practical works are designed
#to take away your freedom to share and change the works.  By contrast,
#the GNU General Public License is intended to guarantee your freedom to
#share and change all versions of a program--to make sure it remains free
#software for all its users.  We, the Free Software Foundation, use the
#GNU General Public License for most of our software; it applies also to
#any other work released this way by its authors.  You can apply it to
#your programs, too.
#
#  When we speak of free software, we are referring to freedom, not
#price.  Our General Public Licenses are designed to make sure that you
#have the freedom to distribute copies of free software (and charge for
#them if you wish), that you receive source code or can get it if you
#want it, that you can change the software or use pieces of it in new
#free programs, and that you know you can do these things.
#
#  To protect your rights, we need to prevent others from denying you
#these rights or asking you to surrender the rights.  Therefore, you have
#certain responsibilities if you distribute copies of the software, or if
#you modify it: responsibilities to respect the freedom of others.
#
#  For example, if you distribute copies of such a program, whether
#gratis or for a fee, you must pass on to the recipients the same
#freedoms that you received.  You must make sure that they, too, receive
#or can get the source code.  And you must show them these terms so they
#know their rights.
#
#  Developers that use the GNU GPL protect your rights with two steps:
#(1) assert copyright on the software, and (2) offer you this License
#giving you legal permission to copy, distribute and/or modify it.
#
#  For the developers' and authors' protection, the GPL clearly explains
#that there is no warranty for this free software.  For both users' and
#authors' sake, the GPL requires that modified versions be marked as
#changed, so that their problems will not be attributed erroneously to
#authors of previous versions.
#
#  Some devices are designed to deny users access to install or run
#modified versions of the software inside them, although the manufacturer
#can do so.  This is fundamentally incompatible with the aim of
#protecting users' freedom to change the software.  The systematic
#pattern of such abuse occurs in the area of products for individuals to
#use, which is precisely where it is most unacceptable.  Therefore, we
#have designed this version of the GPL to prohibit the practice for those
#products.  If such problems arise substantially in other domains, we
#stand ready to extend this provision to those domains in future versions
#of the GPL, as needed to protect the freedom of users.
#
#  Finally, every program is threatened constantly by software patents.
#States should not allow patents to restrict development and use of
#software on general-purpose computers, but in those that do, we wish to
#avoid the special danger that patents applied to a free program could
#make it effectively proprietary.  To prevent this, the GPL assures that
#patents cannot be used to render the program non-free.
#
#  The precise terms and conditions for copying, distribution and
#modification follow.
#
#                       TERMS AND CONDITIONS
#
#  0. Definitions.
#
#  "This License" refers to version 3 of the GNU General Public License.
#
#  "Copyright" also means copyright-like laws that apply to other kinds of
#works, such as semiconductor masks.
#
#  "The Program" refers to any copyrightable work licensed under this
#License.  Each licensee is addressed as "you".  "Licensees" and
#"recipients" may be individuals or organizations.
#
#  To "modify" a work means to copy from or adapt all or part of the work
#in a fashion requiring copyright permission, other than the making of an
#exact copy.  The resulting work is called a "modified version" of the
#earlier work or a work "based on" the earlier work.
#
#  A "covered work" means either the unmodified Program or a work based
#on the Program.
#
#  To "propagate" a work means to do anything with it that, without
#permission, would make you directly or secondarily liable for
#infringement under applicable copyright law, except executing it on a
#computer or modifying a private copy.  Propagation includes copying,
#distribution (with or without modification), making available to the
#public, and in some countries other activities as well.
#
#  To "convey" a work means any kind of propagation that enables other
#parties to make or receive copies.  Mere interaction with a user through
#a computer network, with no transfer of a copy, is not conveying.
#
#  An interactive user interface displays "Appropriate Legal Notices"
#to the extent that it includes a convenient and prominently visible
#feature that (1) displays an appropriate copyright notice, and (2)
#tells the user that there is no warranty for the work (except to the
#extent that warranties are provided), that licensees may convey the
#work under this License, and how to view a copy of this License.  If
#the interface presents a list of user commands or options, such as a
#menu, a prominent item in the list meets this criterion.
#
#  1. Source Code.
#
#  The "source code" for a work means the preferred form of the work
#for making modifications to it.  "Object code" means any non-source
#form of a work.
#
#  A "Standard Interface" means an interface that either is an official
#standard defined by a recognized standards body, or, in the case of
#interfaces specified for a particular programming language, one that
#is widely used among developers working in that language.
#
#  The "System Libraries" of an executable work include anything, other
#than the work as a whole, that (a) is included in the normal form of
#packaging a Major Component, but which is not part of that Major
#Component, and (b) serves only to enable use of the work with that
#Major Component, or to implement a Standard Interface for which an
#implementation is available to the public in source code form.  A
#"Major Component", in this context, means a major essential component
#(kernel, window system, and so on) of the specific operating system
#(if any) on which the executable work runs, or a compiler used to
#produce the work, or an object code interpreter used to run it.
#
#  The "Corresponding Source" for a work in object code form means all
#the source code needed to generate, install, and (for an executable
#work) run the object code and to modify the work, including scripts to
#control those activities.  However, it does not include the work's
#System Libraries, or general-purpose tools or generally available free
#programs which are used unmodified in performing those activities but
#which are not part of the work.  For example, Corresponding Source
#includes interface definition files associated with source files for
#the work, and the source code for shared libraries and dynamically
#linked subprograms that the work is specifically designed to require,
#such as by intimate data communication or control flow between those
#subprograms and other parts of the work.
#
#  The Corresponding Source need not include anything that users
#can regenerate automatically from other parts of the Corresponding
#Source.
#
#  The Corresponding Source for a work in source code form is that
#same work.
#
#  2. Basic Permissions.
#
#  All rights granted under this License are granted for the term of
#copyright on the Program, and are irrevocable provided the stated
#conditions are met.  This License explicitly affirms your unlimited
#permission to run the unmodified Program.  The output from running a
#covered work is covered by this License only if the output, given its
#content, constitutes a covered work.  This License acknowledges your
#rights of fair use or other equivalent, as provided by copyright law.
#
#  You may make, run and propagate covered works that you do not
#convey, without conditions so long as your license otherwise remains
#in force.  You may convey covered works to others for the sole purpose
#of having them make modifications exclusively for you, or provide you
#with facilities for running those works, provided that you comply with
#the terms of this License in conveying all material for which you do
#not control copyright.  Those thus making or running the covered works
#for you must do so exclusively on your behalf, under your direction
#and control, on terms that prohibit them from making any copies of
#your copyrighted material outside their relationship with you.
#
#  Conveying under any other circumstances is permitted solely under
#the conditions stated below.  Sublicensing is not allowed; section 10
#makes it unnecessary.
#
#  3. Protecting Users' Legal Rights From Anti-Circumvention Law.
#
#  No covered work shall be deemed part of an effective technological
#measure under any applicable law fulfilling obligations under article
#11 of the WIPO copyright treaty adopted on 20 December 1996, or
#similar laws prohibiting or restricting circumvention of such
#measures.
#
#  When you convey a covered work, you waive any legal power to forbid
#circumvention of technological measures to the extent such circumvention
#is effected by exercising rights under this License with respect to
#the covered work, and you disclaim any intention to limit operation or
#modification of the work as a means of enforcing, against the work's
#users, your or third parties' legal rights to forbid circumvention of
#technological measures.
#
#  4. Conveying Verbatim Copies.
#
#  You may convey verbatim copies of the Program's source code as you
#receive it, in any medium, provided that you conspicuously and
#appropriately publish on each copy an appropriate copyright notice;
#keep intact all notices stating that this License and any
#non-permissive terms added in accord with section 7 apply to the code;
#keep intact all notices of the absence of any warranty; and give all
#recipients a copy of this License along with the Program.
#
#  You may charge any price or no price for each copy that you convey,
#and you may offer support or warranty protection for a fee.
#
#  5. Conveying Modified Source Versions.
#
#  You may convey a work based on the Program, or the modifications to
#produce it from the Program, in the form of source code under the
#terms of section 4, provided that you also meet all of these conditions:
#
#    a) The work must carry prominent notices stating that you modified
#    it, and giving a relevant date.
#
#    b) The work must carry prominent notices stating that it is
#    released under this License and any conditions added under section
#    7.  This requirement modifies the requirement in section 4 to
#    "keep intact all notices".
#
#    c) You must license the entire work, as a whole, under this
#    License to anyone who comes into possession of a copy.  This
#    License will therefore apply, along with any applicable section 7
#    additional terms, to the whole of the work, and all its parts,
#    regardless of how they are packaged.  This License gives no
#    permission to license the work in any other way, but it does not
#    invalidate such permission if you have separately received it.
#
#    d) If the work has interactive user interfaces, each must display
#    Appropriate Legal Notices; however, if the Program has interactive
#    interfaces that do not display Appropriate Legal Notices, your
#    work need not make them do so.
#
#  A compilation of a covered work with other separate and independent
#works, which are not by their nature extensions of the covered work,
#and which are not combined with it such as to form a larger program,
#in or on a volume of a storage or distribution medium, is called an
#"aggregate" if the compilation and its resulting copyright are not
#used to limit the access or legal rights of the compilation's users
#beyond what the individual works permit.  Inclusion of a covered work
#in an aggregate does not cause this License to apply to the other
#parts of the aggregate.
#
#  6. Conveying Non-Source Forms.
#
#  You may convey a covered work in object code form under the terms
#of sections 4 and 5, provided that you also convey the
#machine-readable Corresponding Source under the terms of this License,
#in one of these ways:
#
#    a) Convey the object code in, or embodied in, a physical product
#    (including a physical distribution medium), accompanied by the
#    Corresponding Source fixed on a durable physical medium
#    customarily used for software interchange.
#
#    b) Convey the object code in, or embodied in, a physical product
#    (including a physical distribution medium), accompanied by a
#    written offer, valid for at least three years and valid for as
#    long as you offer spare parts or customer support for that product
#    model, to give anyone who possesses the object code either (1) a
#    copy of the Corresponding Source for all the software in the
#    product that is covered by this License, on a durable physical
#    medium customarily used for software interchange, for a price no
#    more than your reasonable cost of physically performing this
#    conveying of source, or (2) access to copy the
#    Corresponding Source from a network server at no charge.
#
#    c) Convey individual copies of the object code with a copy of the
#    written offer to provide the Corresponding Source.  This
#    alternative is allowed only occasionally and noncommercially, and
#    only if you received the object code with such an offer, in accord
#    with subsection 6b.
#
#    d) Convey the object code by offering access from a designated
#    place (gratis or for a charge), and offer equivalent access to the
#    Corresponding Source in the same way through the same place at no
#    further charge.  You need not require recipients to copy the
#    Corresponding Source along with the object code.  If the place to
#    copy the object code is a network server, the Corresponding Source
#    may be on a different server (operated by you or a third party)
#    that supports equivalent copying facilities, provided you maintain
#    clear directions next to the object code saying where to find the
#    Corresponding Source.  Regardless of what server hosts the
#    Corresponding Source, you remain obligated to ensure that it is
#    available for as long as needed to satisfy these requirements.
#
#    e) Convey the object code using peer-to-peer transmission, provided
#    you inform other peers where the object code and Corresponding
#    Source of the work are being offered to the general public at no
#    charge under subsection 6d.
#
#  A separable portion of the object code, whose source code is excluded
#from the Corresponding Source as a System Library, need not be
#included in conveying the object code work.
#
#  A "User Product" is either (1) a "consumer product", which means any
#tangible personal property which is normally used for personal, family,
#or household purposes, or (2) anything designed or sold for incorporation
#into a dwelling.  In determining whether a product is a consumer product,
#doubtful cases shall be resolved in favor of coverage.  For a particular
#product received by a particular user, "normally used" refers to a
#typical or common use of that class of product, regardless of the status
#of the particular user or of the way in which the particular user
#actually uses, or expects or is expected to use, the product.  A product
#is a consumer product regardless of whether the product has substantial
#commercial, industrial or non-consumer uses, unless such uses represent
#the only significant mode of use of the product.
#
#  "Installation Information" for a User Product means any methods,
#procedures, authorization keys, or other information required to install
#and execute modified versions of a covered work in that User Product from
#a modified version of its Corresponding Source.  The information must
#suffice to ensure that the continued functioning of the modified object
#code is in no case prevented or interfered with solely because
#modification has been made.
#
#  If you convey an object code work under this section in, or with, or
#specifically for use in, a User Product, and the conveying occurs as
#part of a transaction in which the right of possession and use of the
#User Product is transferred to the recipient in perpetuity or for a
#fixed term (regardless of how the transaction is characterized), the
#Corresponding Source conveyed under this section must be accompanied
#by the Installation Information.  But this requirement does not apply
#if neither you nor any third party retains the ability to install
#modified object code on the User Product (for example, the work has
#been installed in ROM).
#
#  The requirement to provide Installation Information does not include a
#requirement to continue to provide support service, warranty, or updates
#for a work that has been modified or installed by the recipient, or for
#the User Product in which it has been modified or installed.  Access to a
#network may be denied when the modification itself materially and
#adversely affects the operation of the network or violates the rules and
#protocols for communication across the network.
#
#  Corresponding Source conveyed, and Installation Information provided,
#in accord with this section must be in a format that is publicly
#documented (and with an implementation available to the public in
#source code form), and must require no special password or key for
#unpacking, reading or copying.
#
#  7. Additional Terms.
#
#  "Additional permissions" are terms that supplement the terms of this
#License by making exceptions from one or more of its conditions.
#Additional permissions that are applicable to the entire Program shall
#be treated as though they were included in this License, to the extent
#that they are valid under applicable law.  If additional permissions
#apply only to part of the Program, that part may be used separately
#under those permissions, but the entire Program remains governed by
#this License without regard to the additional permissions.
#
#  When you convey a copy of a covered work, you may at your option
#remove any additional permissions from that copy, or from any part of
#it.  (Additional permissions may be written to require their own
#removal in certain cases when you modify the work.)  You may place
#additional permissions on material, added by you to a covered work,
#for which you have or can give appropriate copyright permission.
#
#  Notwithstanding any other provision of this License, for material you
#add to a covered work, you may (if authorized by the copyright holders of
#that material) supplement the terms of this License with terms:
#
#    a) Disclaiming warranty or limiting liability differently from the
#    terms of sections 15 and 16 of this License; or
#
#    b) Requiring preservation of specified reasonable legal notices or
#    author attributions in that material or in the Appropriate Legal
#    Notices displayed by works containing it; or
#
#    c) Prohibiting misrepresentation of the origin of that material, or
#    requiring that modified versions of such material be marked in
#    reasonable ways as different from the original version; or
#
#    d) Limiting the use for publicity purposes of names of licensors or
#    authors of the material; or
#
#    e) Declining to grant rights under trademark law for use of some
#    trade names, trademarks, or service marks; or
#
#    f) Requiring indemnification of licensors and authors of that
#    material by anyone who conveys the material (or modified versions of
#    it) with contractual assumptions of liability to the recipient, for
#    any liability that these contractual assumptions directly impose on
#    those licensors and authors.
#
#  All other non-permissive additional terms are considered "further
#restrictions" within the meaning of section 10.  If the Program as you
#received it, or any part of it, contains a notice stating that it is
#governed by this License along with a term that is a further
#restriction, you may remove that term.  If a license document contains
#a further restriction but permits relicensing or conveying under this
#License, you may add to a covered work material governed by the terms
#of that license document, provided that the further restriction does
#not survive such relicensing or conveying.
#
#  If you add terms to a covered work in accord with this section, you
#must place, in the relevant source files, a statement of the
#additional terms that apply to those files, or a notice indicating
#where to find the applicable terms.
#
#  Additional terms, permissive or non-permissive, may be stated in the
#form of a separately written license, or stated as exceptions;
#the above requirements apply either way.
#
#  8. Termination.
#
#  You may not propagate or modify a covered work except as expressly
#provided under this License.  Any attempt otherwise to propagate or
#modify it is void, and will automatically terminate your rights under
#this License (including any patent licenses granted under the third
#paragraph of section 11).
#
#  However, if you cease all violation of this License, then your
#license from a particular copyright holder is reinstated (a)
#provisionally, unless and until the copyright holder explicitly and
#finally terminates your license, and (b) permanently, if the copyright
#holder fails to notify you of the violation by some reasonable means
#prior to 60 days after the cessation.
#
#  Moreover, your license from a particular copyright holder is
#reinstated permanently if the copyright holder notifies you of the
#violation by some reasonable means, this is the first time you have
#received notice of violation of this License (for any work) from that
#copyright holder, and you cure the violation prior to 30 days after
#your receipt of the notice.
#
#  Termination of your rights under this section does not terminate the
#licenses of parties who have received copies or rights from you under
#this License.  If your rights have been terminated and not permanently
#reinstated, you do not qualify to receive new licenses for the same
#material under section 10.
#
#  9. Acceptance Not Required for Having Copies.
#
#  You are not required to accept this License in order to receive or
#run a copy of the Program.  Ancillary propagation of a covered work
#occurring solely as a consequence of using peer-to-peer transmission
#to receive a copy likewise does not require acceptance.  However,
#nothing other than this License grants you permission to propagate or
#modify any covered work.  These actions infringe copyright if you do
#not accept this License.  Therefore, by modifying or propagating a
#covered work, you indicate your acceptance of this License to do so.
#
#  10. Automatic Licensing of Downstream Recipients.
#
#  Each time you convey a covered work, the recipient automatically
#receives a license from the original licensors, to run, modify and
#propagate that work, subject to this License.  You are not responsible
#for enforcing compliance by third parties with this License.
#
#  An "entity transaction" is a transaction transferring control of an
#organization, or substantially all assets of one, or subdividing an
#organization, or merging organizations.  If propagation of a covered
#work results from an entity transaction, each party to that
#transaction who receives a copy of the work also receives whatever
#licenses to the work the party's predecessor in interest had or could
#give under the previous paragraph, plus a right to possession of the
#Corresponding Source of the work from the predecessor in interest, if
#the predecessor has it or can get it with reasonable efforts.
#
#  You may not impose any further restrictions on the exercise of the
#rights granted or affirmed under this License.  For example, you may
#not impose a license fee, royalty, or other charge for exercise of
#rights granted under this License, and you may not initiate litigation
#(including a cross-claim or counterclaim in a lawsuit) alleging that
#any patent claim is infringed by making, using, selling, offering for
#sale, or importing the Program or any portion of it.
#
#  11. Patents.
#
#  A "contributor" is a copyright holder who authorizes use under this
#License of the Program or a work on which the Program is based.  The
#work thus licensed is called the contributor's "contributor version".
#
#  A contributor's "essential patent claims" are all patent claims
#owned or controlled by the contributor, whether already acquired or
#hereafter acquired, that would be infringed by some manner, permitted
#by this License, of making, using, or selling its contributor version,
#but do not include claims that would be infringed only as a
#consequence of further modification of the contributor version.  For
#purposes of this definition, "control" includes the right to grant
#patent sublicenses in a manner consistent with the requirements of
#this License.
#
#  Each contributor grants you a non-exclusive, worldwide, royalty-free
#patent license under the contributor's essential patent claims, to
#make, use, sell, offer for sale, import and otherwise run, modify and
#propagate the contents of its contributor version.
#
#  In the following three paragraphs, a "patent license" is any express
#agreement or commitment, however denominated, not to enforce a patent
#(such as an express permission to practice a patent or covenant not to
#sue for patent infringement).  To "grant" such a patent license to a
#party means to make such an agreement or commitment not to enforce a
#patent against the party.
#
#  If you convey a covered work, knowingly relying on a patent license,
#and the Corresponding Source of the work is not available for anyone
#to copy, free of charge and under the terms of this License, through a
#publicly available network server or other readily accessible means,
#then you must either (1) cause the Corresponding Source to be so
#available, or (2) arrange to deprive yourself of the benefit of the
#patent license for this particular work, or (3) arrange, in a manner
#consistent with the requirements of this License, to extend the patent
#license to downstream recipients.  "Knowingly relying" means you have
#actual knowledge that, but for the patent license, your conveying the
#covered work in a country, or your recipient's use of the covered work
#in a country, would infringe one or more identifiable patents in that
#country that you have reason to believe are valid.
#
#  If, pursuant to or in connection with a single transaction or
#arrangement, you convey, or propagate by procuring conveyance of, a
#covered work, and grant a patent license to some of the parties
#receiving the covered work authorizing them to use, propagate, modify
#or convey a specific copy of the covered work, then the patent license
#you grant is automatically extended to all recipients of the covered
#work and works based on it.
#
#  A patent license is "discriminatory" if it does not include within
#the scope of its coverage, prohibits the exercise of, or is
#conditioned on the non-exercise of one or more of the rights that are
#specifically granted under this License.  You may not convey a covered
#work if you are a party to an arrangement with a third party that is
#in the business of distributing software, under which you make payment
#to the third party based on the extent of your activity of conveying
#the work, and under which the third party grants, to any of the
#parties who would receive the covered work from you, a discriminatory
#patent license (a) in connection with copies of the covered work
#conveyed by you (or copies made from those copies), or (b) primarily
#for and in connection with specific products or compilations that
#contain the covered work, unless you entered into that arrangement,
#or that patent license was granted, prior to 28 March 2007.
#
#  Nothing in this License shall be construed as excluding or limiting
#any implied license or other defenses to infringement that may
#otherwise be available to you under applicable patent law.
#
#  12. No Surrender of Others' Freedom.
#
#  If conditions are imposed on you (whether by court order, agreement or
#otherwise) that contradict the conditions of this License, they do not
#excuse you from the conditions of this License.  If you cannot convey a
#covered work so as to satisfy simultaneously your obligations under this
#License and any other pertinent obligations, then as a consequence you may
#not convey it at all.  For example, if you agree to terms that obligate you
#to collect a royalty for further conveying from those to whom you convey
#the Program, the only way you could satisfy both those terms and this
#License would be to refrain entirely from conveying the Program.
#
#  13. Use with the GNU Affero General Public License.
#
#  Notwithstanding any other provision of this License, you have
#permission to link or combine any covered work with a work licensed
#under version 3 of the GNU Affero General Public License into a single
#combined work, and to convey the resulting work.  The terms of this
#License will continue to apply to the part which is the covered work,
#but the special requirements of the GNU Affero General Public License,
#section 13, concerning interaction through a network will apply to the
#combination as such.
#
#  14. Revised Versions of this License.
#
#  The Free Software Foundation may publish revised and/or new versions of
#the GNU General Public License from time to time.  Such new versions will
#be similar in spirit to the present version, but may differ in detail to
#address new problems or concerns.
#
#  Each version is given a distinguishing version number.  If the
#Program specifies that a certain numbered version of the GNU General
#Public License "or any later version" applies to it, you have the
#option of following the terms and conditions either of that numbered
#version or of any later version published by the Free Software
#Foundation.  If the Program does not specify a version number of the
#GNU General Public License, you may choose any version ever published
#by the Free Software Foundation.
#
#  If the Program specifies that a proxy can decide which future
#versions of the GNU General Public License can be used, that proxy's
#public statement of acceptance of a version permanently authorizes you
#to choose that version for the Program.
#
#  Later license versions may give you additional or different
#permissions.  However, no additional obligations are imposed on any
#author or copyright holder as a result of your choosing to follow a
#later version.
#
#  15. Disclaimer of Warranty.
#
#  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
#APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
#HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
#OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
#THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
#IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
#ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
#
#  16. Limitation of Liability.
#
#  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
#WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
#THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
#GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
#USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
#DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
#PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
#EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
#SUCH DAMAGES.
#
#  17. Interpretation of Sections 15 and 16.
#
#  If the disclaimer of warranty and limitation of liability provided
#above cannot be given local legal effect according to their terms,
#reviewing courts shall apply local law that most closely approximates
#an absolute waiver of all civil liability in connection with the
#Program, unless a warranty or assumption of liability accompanies a
#copy of the Program in return for a fee.
#
#                     END OF TERMS AND CONDITIONS
#
#            How to Apply These Terms to Your New Programs
#
#  If you develop a new program, and you want it to be of the greatest
#possible use to the public, the best way to achieve this is to make it
#free software which everyone can redistribute and change under these terms.
#
#  To do so, attach the following notices to the program.  It is safest
#to attach them to the start of each source file to most effectively
#state the exclusion of warranty; and each file should have at least
#the "copyright" line and a pointer to where the full notice is found.
#
#    <one line to give the program's name and a brief idea of what it does.>
#    Copyright (C) <year>  <name of author>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#Also add information on how to contact you by electronic and paper mail.
#
#  If the program does terminal interaction, make it output a short
#notice like this when it starts in an interactive mode:
#
#    <program>  Copyright (C) <year>  <name of author>
#    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
#    This is free software, and you are welcome to redistribute it
#    under certain conditions; type `show c' for details.
#
#The hypothetical commands `show w' and `show c' should show the appropriate
#parts of the General Public License.  Of course, your program's commands
#might be different; for a GUI interface, you would use an "about box".
#
#  You should also get your employer (if you work as a programmer) or school,
#if any, to sign a "copyright disclaimer" for the program, if necessary.
#For more information on this, and how to apply and follow the GNU GPL, see
#<http://www.gnu.org/licenses/>.
#
#  The GNU General Public License does not permit incorporating your program
#into proprietary programs.  If your program is a subroutine library, you
#may consider it more useful to permit linking proprietary applications with
#the library.  If this is what you want to do, use the GNU Lesser General
#Public License instead of this License.  But first, please read
#<http://www.gnu.org/philosophy/why-not-lgpl.html>.

#These variables (in main) are used by printVersion()
my $template_version_number = '1.32';
my $software_version_number = '1.1';

##
## Start Main
##

use strict;
use Getopt::Long;

#Declare & initialize variables.  Provide default values here.
my($outfile_suffix); #Not defined on purpose so a user can overwrite the input file
my @input_files         = ();
my $current_output_file = '';
my $help                = 0;
my $version             = 0;
my $force               = 0;
my $mz_play_fraction    = .0005;
my $amp_play_fraction   = .2;
my $snr_play_fraction   = .5;
my $scale_play_distance = 10;
my $use_amp             = 0;
my $use_scale           = 0;
my $use_snr             = 0;
my $mz_file             = '';

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, printVersion, getCommand and usage
my $preserve_args = [@ARGV];  #Preserve the agruments for getCommand
my $verbose       = 0;
my $quiet         = 0;
my $DEBUG         = 0;

my $GetOptHash =
  {'i|input-file=s'     => sub {push(@input_files,   #REQUIRED unless <> is
				     sglob($_[1]))}, #         supplied
   '<>'                 => sub {push(@input_files,   #REQUIRED unless -i is
				     sglob($_[0]))}, #         supplied
   'm|mz-file=s'        => \$mz_file,                #REQUIRED
   'b|bin-center-egde-width-fraction=s' => \$mz_play_fraction,
   'a|use-amplitudes!'  => \$use_amp,                #OPTIONAL [Off]
   's|use-scales!'      => \$use_scale,              #OPTIONAL [Off]
   'n|use-signal2noise!'=> \$use_snr,                #OPTIONAL [Off]
   'o|outfile-suffix=s' => \$outfile_suffix,         #OPTIONAL [undef]
   'f|force!'           => \$force,                  #OPTIONAL [Off]
   'v|verbose!'         => \$verbose,                #OPTIONAL [Off]
   'q|quiet!'           => \$quiet,                  #OPTIONAL [Off]
   'h|help!'            => \$help,                   #OPTIONAL [Off]
   'debug!'             => \$DEBUG,                  #OPTIONAL [Off]
   'version!'           => \$version,                #OPTIONAL [Off]
  };

#If there are no arguments and no files directed or piped in
if(scalar(@ARGV) == 0 && isStandardInputFromTerminal())
  {
    usage();
    exit(0);
  }

#Get the input options
GetOptions(%$GetOptHash);

#Print the debug mode (it checks the value of the DEBUG global variable)
debug("Debug mode on.");

#If the user has asked for help, call the help subroutine
if($help)
  {
    help();
    exit(0);
  }

#If the user has asked for the software version, print it
if($version)
  {
    printVersion();
    exit(0);
  }

#Check validity of verbosity options
if($verbose && $quiet)
  {
    $quiet = 0;
    error("You cannot supply verbose and quiet flags at the same time.");
    exit(1);
  }

#Put standard input into the input_files array if standard input has been redirected in
if(!isStandardInputFromTerminal())
  {
    push(@input_files,'-');

    #Warn the user about the naming of the outfile when using STDIN
    if(defined($outfile_suffix))
      {warning("Input on STDIN detected along with an outfile suffix.  Your ",
	       "output file will be named STDIN$outfile_suffix")}
  }

#Make sure there is input
if(scalar(@input_files) == 0)
  {
    error("No input files detected.");
    usage(1);
    exit(2);
  }

#Check to make sure previously generated output files won't be over-written
if(!$force && defined($outfile_suffix))
  {
    my $existing_outfiles = [];
    foreach my $output_file (map {($_ eq '-' ? 'STDIN' : $_) . $outfile_suffix}
			     @input_files)
      {push(@$existing_outfiles,$output_file) if(-e $output_file)}

    if(scalar(@$existing_outfiles))
      {
	error("The output files: [@$existing_outfiles] already exist.  ",
	      "Use -f to force overwrite.  E.g.\n\t",
	      getCommand(1),' --force');
	exit(3);
      }
  }

if(isStandardOutputToTerminal() && !defined($outfile_suffix))
  {verbose("NOTE: VerboseOverMe functionality has been altered to yield ",
	   "clean STDOUT output.")}

if(!defined($mz_file) || $mz_file eq '')
  {
    error("No mz-file (-m) was supplied.");
    usage(1);
    exit(4);
  }

verbose("Run conditions: ",getCommand(1),"\n");


##
## Obtain the MZs where we want to extract peaks
##

my @target_mzs = ();
if(!open(MZS,$mz_file))
  {
    error("Unable to open mz-file: [$mz_file]: $!");
    exit(5);
  }
else
  {
    verbose("[$mz_file] Opened input file.");

    my $line_num = 0;

    while(getLine(*MZS))
      {
	$line_num++;

	verboseOverMe("[$mz_file] Reading line [$line_num].")
	  unless($line_num % 1000);

	next if(/^\s*$/ || /^#/);
	chomp;

	if(/[^\d\.\s]/)
	  {
	    my @errs = (/([^\d\.\s])/g);
	    warning("Non-numeric characters: [",join(',',@errs),
		    "] found on line: [$line_num]: [$_].  Skipping.");
	    next;
	  }

	chomp;
	s/^ +//;
	s/ +$//;
	push(@target_mzs,$_);
      }

    close(MZS);

    verbose("[$mz_file] Input file done.");
  }

@target_mzs = sort {$a <=> $b} @target_mzs;






my $data = {};    #{$input_file => [[mz,amplitude,signal2noise,scale]]}
my $unordered_data = {};
my $all_mzs = {}; #{mz# => count within binning range}
my $intensity_only = 0;

#If output is going to STDOUT instead of output files with different extensions
if(!defined($outfile_suffix))
  {verbose("[STDOUT] Opened for all output.")}

#For each input file
foreach my $input_file (@input_files)
  {
    #Open the input file
    if(!open(INPUT,$input_file))
      {
	#Report an error and iterate if there was an error
	error("Unable to open input file: [$input_file]\n$!");
	next;
      }
    else
      {verboseOverMe("[",
		     ($input_file eq '-' ? 'STDIN' : $input_file),
		     "] Opened input file.")}

    #Keep track of input file's line number
    my $line_num   = 0;

    #For each line in the current input file
    while(getLine(*INPUT))
      {
	$line_num++;

	verboseOverMe("[",
		      ($input_file eq '-' ? 'STDIN' : $input_file),
		      "] Reading line $line_num.")
	  unless($line_num % 1000);

	#Skip this line if it's a header, empty, or commented
	next if(/peak/ || /^\s*$/ || /^#/);
	chomp;

	#See if the line contains anything other than numbers formatted like
	#this: 1.4e-10 or .0005 or e-22  (note a loose pattern match is used)
	if(/[^\d\.\se]/i)
	  {
	    warning("Non-numeric characters found on line: [$line_num]: [$_] ",
		    "Skipping.");
	    next;
	  }

	s/^ +//;
	s/ +$//;
	push(@{$unordered_data->{$input_file}},[split(/\s+/,$_)]);

	if(scalar(@{$unordered_data->{$input_file}->[-1]}) < 2)
	  {
	    warning("Not enough columns on line [$line_num] of input file: ",
		    "[$input_file]: [$_].  Skipping.");
	    next;
	  }

	#This is a kluge to allow files containing only mz and intensity
	#We're editing the array read in to contain all the columns of the
	#original format by inserting bogus values ("1's").
	if(scalar(@{$unordered_data->{$input_file}->[-1]}) == 2)
	  {
	    $intensity_only = 1;

	    #Put an arbitrary index on the front
	    unshift(@{$unordered_data->{$input_file}->[-1]},1);
	    #Tack on a bogus SNR and scale value
	    push(@{$unordered_data->{$input_file}->[-1]},(1,1))
	  }

	#Now check to see if this data point is inside the range of one of the
	#target mz values and that it is a peak.  If not, pop off the data
	#point and go to the next iteration
	if(!inRange($unordered_data->{$input_file}->[-1]->[1],
		    $mz_play_fraction,
		    \@target_mzs))
	  {
	    #All I should need to do is pop & skip this point if it's not
	    #inside the window of one of the target mzs.
	    pop(@{$unordered_data->{$input_file}});
	    next;
	  }

	$all_mzs->{$unordered_data->{$input_file}->[-1]->[1]}++;
      }

    #I inserted the following foreach so that I could order the peak data Rama
    #gave to me in an unordered fashion.  basically, I split the while loop
    #above in half to read all the data points in and then go through them
    #again here.  I changed the above while loop to insert into the temporary
    #"unordered data" array and then push everything onto the final data array
    #here
    foreach my $array (sort {$a->[1] <=> $b->[1]}
		       @{$unordered_data->{$input_file}})
      {
	push(@{$data->{$input_file}},$array);

	#If we've kept more than one data point thus far, make sure we're
	#keeping the most intense one within range of the same target MZ.
	if(scalar(@{$data->{$input_file}}) > 1)
	  {
	    #Obtain the last two mz values read in
	    my $last_mz = $data->{$input_file}->[-1]->[1];
	    my $prev_mz = $data->{$input_file}->[-2]->[1];
	    #Calculate the lower range of the larger value (assumed to be last)
#	    my $lbound = $last_mz - ($mz_play_fraction * $last_mz);

	    #Obtain the last two amplitude values read in
	    my $last_amp = $data->{$input_file}->[-1]->[2];
	    my $prev_amp = $data->{$input_file}->[-2]->[2];
#	    my $aplay = $amp_play_fraction *
#	      ($last_amp > $prev_amp ? $last_amp : $prev_amp);
#
#	    #Obtain the last two SNR values read in
#	    my $last_snr = $data->{$input_file}->[-1]->[3];
#	    my $prev_snr = $data->{$input_file}->[-2]->[3];
#	    my $splay = $snr_play_fraction *
#	      ($last_snr > $prev_snr ? $last_snr : $prev_snr);
#
#	    #Obtain the last two scales read in
#	    my $last_scale = $data->{$input_file}->[-1]->[4];
#	    my $prev_scale = $data->{$input_file}->[-2]->[4];
#	    my $play = $scale_play_distance;

	    #Assumes ascending MZ values in the file
	    #If both of the last mzs are in range of the same target mz
	    #And the prev_mz and last_mz are in range of one another
	    #This assumes the target values don't overlap - do not use this for
	    #serious calculations until this is accounted for!
	    if(#((
	       inRange($prev_mz,$mz_play_fraction,\@target_mzs) &&
		 inRange($prev_mz,$mz_play_fraction,\@target_mzs) ==
		 inRange($last_mz,$mz_play_fraction,\@target_mzs)    #) ||
#		$prev_mz > $lbound) &&
#	       (!$use_amp || ($prev_amp >= ($last_amp - $aplay) &&
#			      $prev_amp <= ($last_amp + $aplay))) &&
#	       (!$use_snr || ($prev_snr >= ($last_snr - $splay) &&
#			      $prev_snr <= ($last_snr + $splay))) &&
#	       (!$use_scale || ($prev_scale >= ($last_scale - $play) &&
#				$prev_scale <= ($last_scale + $play)))
	      )
	      {
#		warning("CLOSE PEAKS FOUND IN $input_file: $prev_mz ",
#			"AND $last_mz: @{$data->{$input_file}->[-2]} | ",
#			"@{$data->{$input_file}->[-1]}\n");

		#I commented out the above because I expect to find
		#close points now
		my $last = $data->{$input_file}->[-1];
		my $prev = $data->{$input_file}->[-2];

		pop(@{$data->{$input_file}});
		pop(@{$data->{$input_file}});

		if($last_amp > $prev_amp)
		  {
		    debug("Choosing $last_mz over $prev_mz");
		    push(@{$data->{$input_file}},[@$last]);
		    $all_mzs->{$prev_mz}--;
		  }
		else
		  {
		    debug("Choosing $prev_mz over $last_mz");
		    push(@{$data->{$input_file}},[@$prev]);
		    $all_mzs->{$last_mz}--;
		  }
	      }
	  }
      }

    close(INPUT);

    debug(scalar(@{$data->{$input_file}})," DATA POINTS FOR [$input_file]: [",join(', ',map {"$_->[1]=$_->[2]"} @{$data->{$input_file}}),"].");

    verbose("[",
	    ($input_file eq '-' ? 'STDIN' : $input_file),
	    '] Input file done.  Time taken: [',
	    scalar(markTime()),
	    " Seconds].");
  }

##Now correct the all_mzs for ones that were removed.  Ideally, anything that was decremented above is at 0 in this hash, but if we choose the wrong peak to keep (i.e. the one that's consistent with mz values from others that were kept) then we've got a problem, but I'm going to assume we're always going to get the right peaks.
#foreach my $mz_key (keys(%$all_mzs))
#  {
#    if($all_mzs->{$mz_key} < 1)
#      {
#	debug("Removing tossed mz value: $mz_key\n");
#	delete($all_mzs->{$mz_key});
#      }
#    else #Reset the mz count to 0
#      {$all_mzs->{$mz_key} = 0}
#  }

##Let's see how many other mz values each mz value touches
#foreach my $mz (keys(%$all_mzs))
#  {
#    foreach my $other_mz (sort {$a <=> $b} keys(%$all_mzs))
#      {
#	if($other_mz >= ($mz - ($mz_play_fraction * $mz)) &&
#	   $other_mz <= ($mz + ($mz_play_fraction * $mz)))
#	  {$all_mzs->{$mz}++}
#	elsif($other_mz > $mz)
#	  {last}
#      }
#  }

#print("#MZ\tBin content count given $mz_play_fraction sized bins\n",
#      join("\n",map {"$_\t$all_mzs->{$_}"} sort {$a <=> $b} keys(%$all_mzs)),
#      "\n");

#Procedure to pick final bins:
#Go through the bins in order of decreasing number of contained peaks.
#See if there are break points between different groups of peaks and for each group...
#Choose the center value and add it as a final bin
#Remove values that fall into the bin from the all_mzs hash

#Go through the bins in order of decreasing number of contained peaks.
my $check = {};
my $bin_mz_hash = {};  #Points every mz to the mz of the bin its been assigned
my @final_bins = @target_mzs;

#Print the final bin center values
#print("Bin center values:\n",join("\n",sort {$a <=> $b} @final_bins),"\n\n");

#Now I need to alter the input to put in the bin data
#The first column is actually an artificial bin created by an alignment
#algorithm, so I'm going to replace that with an arbitrary global sequential
#bin number
#First create a bin number hash keyed on the final mz bins chosen above
my $bin_num = 0;
my $bin_num_hash = {};
foreach my $final_bin_mz (sort {$a <=> $b} @final_bins)
  {
    $bin_num++;
    $bin_num_hash->{$final_bin_mz} = $bin_num;
  }

##Now go through each input file
#my $bin_crowding = {};
#foreach my $input_file (keys(%$data))
#  {
#    #If an output file name suffix has been defined
#    if(defined($outfile_suffix))
#      {
#	##
#	## Open and select the next output file
#	##
#
#	#Set the current output file name
#	$current_output_file = ($input_file eq '-' ? 'STDIN' : $input_file)
#	  . $outfile_suffix;
#
#	#Open the output file
#	if(!open(OUTPUT,">$current_output_file"))
#	  {
#	    #Report an error and iterate if there was an error
#	    error("Unable to open output file: [$current_output_file]\n$!");
#	    next;
#	  }
#	else
#	  {verboseOverMe("[$current_output_file] Opened output file.")}
#
#	#Select the output file handle
#	select(OUTPUT);
#      }
#
#    foreach my $row (@{$data->{$input_file}})
#      {
##	error("ONE OF THESE VALUES IS NOT DEFINED: SECOND ELEMENT OF ROW: [$row->[1]] VALUE FOR THAT KEY IN BIN_MZ_HASH: [$bin_mz_hash->{$row->[1]}] OR THAT VALUE IN BIN_NUM_HASH: [$bin_num_hash->{$bin_mz_hash->{$row->[1]}].") if(!defined($row->[1]) || !defined($bin_mz_hash->{$row->[1]}) || !defined($bin_num_hash->{$bin_mz_hash->{$row->[1]}}));
#	my $elem = inRange($row->[1],$mz_play_fraction,\@target_mzs);
#	my $index = 1;
#	foreach(@target_mzs)
#	  {
#	    last if($elem eq $_);
#	    $index++;
#	  }
#	$row->[0] = $index;
#	$bin_crowding->{$input_file}->{$row->[0]}++;
#	print(join("\t",@$row),"\n");
#      }
#
#    #If an output file name suffix is set
#    if(defined($outfile_suffix))
#      {
#	#Select standard out
#	select(STDOUT);
#	#Close the output file handle
#	close(OUTPUT);
#
#	verbose("[$current_output_file] Output file done.");
#      }
#  }


#Now print out a table of the data where each row is a sample and each column is a bin containing sub-columns of intensity, SNR, & scale.  We'll create a 2D hash to contain the table.
my $table_hash = {}; #{$input_file=>{$bin_num=>{INTENSITY=>$intensity,SNR=>$snr,SCALE=>$scale}}}
foreach my $input_file (keys(%$data))
  {
    if(!defined($data->{$input_file}) || ref($data->{$input_file}) ne 'ARRAY')
      {
	error("No array found for input file: [$input_file].");
	next;
      }

    debug("CONTENTS OF TABLE HASH FOR FILE: [$input_file]: [",scalar(@{$data->{$input_file}})," elements]");

    #Fill the table with the existing values
    foreach my $bin_row (@{$data->{$input_file}})
      {
	debug("CONTENTS OF ROW: [@$bin_row].");
	$table_hash->{$input_file}->{$bin_num_hash->{inRange($bin_row->[1],$mz_play_fraction,\@target_mzs)}} =
	  {INTENSITY => $bin_row->[2],
	   SNR       => $bin_row->[3],
	   SCALE     => $bin_row->[4]};
      }

    #Fill in dots for the missing values (some spectra don't have all peaks)
    foreach my $bin (1..$bin_num)
      {$table_hash->{$input_file}->{$bin}={INTENSITY => '.',
					   SNR       => '.',
					   SCALE     => '.'}
	 unless(exists($table_hash->{$input_file}->{$bin}))}
  }

my $largest_filename_size = 0;
foreach my $tfile (sort {$a cmp $b} keys(%$table_hash))
  {$largest_filename_size = length($tfile)
     if(length($tfile) > $largest_filename_size)}

#Print the header
if($intensity_only)
  {
    print("#FILE",' ' x ($largest_filename_size - 5),"\t",
	  join("\t",
	       (map {sigdig($_,6)} sort {$a <=> $b} keys(%$bin_num_hash))),
	  "\n");
    print(' ' x ($largest_filename_size),"\t",join("\t",(1..$bin_num)),"\n");
  }
else
  {
    print(' ' x ($largest_filename_size),"\t\t",
	  (map {sigdig($_,6) . "\t\t"} sort {$a <=> $b} keys(%$bin_num_hash)),
	  "\n");
    print("#FILE                                 \t",
	  (map {"\t$_\_INT\t$_\_SNR\t$_\_SCL"} (1..$bin_num)),
	  "\n");
  }

foreach my $file (sort {$a cmp $b} keys(%$table_hash))
  {
    print($file);
    foreach my $bin (sort {$a <=> $b} keys(%{$table_hash->{$file}}))
      {
	print("\t",sigdig($table_hash->{$file}->{$bin}->{INTENSITY},4),
	      ($intensity_only ? '' :
	       "\t" . sigdig($table_hash->{$file}->{$bin}->{SNR},4) .
	       "\t" . sigdig($table_hash->{$file}->{$bin}->{SCALE},4)));
      }
    print("\n");
  }



##Warnings about multiple peaks from a single spectrum going into the same bin.
#foreach my $file (keys(%$bin_crowding))
#  {
#    foreach my $bin_num (sort {$a <=> $b} keys(%{$bin_crowding->{$file}}))
#      {
#	warning("Bin: [$bin_num] in file: [$file] contains multiple peaks ",
#		"from this spectrum.")
#	  if($bin_crowding->{$file}->{$bin_num} > 1);
#      }
#  }

#Report the number of errors, warnings, and debugs on STDERR
if(!$quiet && ($verbose                     ||
	       $DEBUG                       ||
	       defined($main::error_number) ||
	       defined($main::warning_number)))
  {
    print STDERR ("\n",'Done.  EXIT STATUS: [',
		  'ERRORS: ',
		  ($main::error_number ? $main::error_number : 0),' ',
		  'WARNINGS: ',
		  ($main::warning_number ? $main::warning_number : 0),
		  ($DEBUG ?
		   ' DEBUGS: ' .
		   ($main::debug_number ? $main::debug_number : 0) : ''),' ',
		  'TIME: ',scalar(markTime(0)),"s]\n");

    if($main::error_number || $main::warning_number)
      {print STDERR ("Scroll up to inspect errors and warnings.\n")}
  }

##
## End Main
##






























##
## Subroutines
##

#Returns the closest target peak location which the submitted MZ is in range of
#(assumes target mzs array is in order from smallest to largest mz).  Target MZ
#returned when the distance is equal is not guarantted to be reproducible
#Returns 0 if the MZ is not in range of a target MZ ratio.
sub inRange
  {
    my $mz               = $_[0];
    my $mz_play_fraction = $_[1];
    my $target_mzs       = $_[2];
    my @in_range         = ();

    if(!defined($mz) || $mz !~ /^\d*\.?\d+/)
      {
	error("No MZ was supplied!");
	return(0);
      }
    elsif(!defined($mz_play_fraction) || $mz_play_fraction !~ /^\d*\.?\d+/)
      {
	error("No MZ play fraction was supplied!");
	return(0);
      }

#    debug("Target MZs sent in: [@$target_mzs].\n");

    my $index = 0;
    #Assumes target MZ's are in order from smallest to largest
    while($index < scalar(@$target_mzs) &&
	  ($mz > ($target_mzs->[$index] -
		  ($target_mzs->[$index] * $mz_play_fraction))))
      {
#	debug("TESTING THIS: [$mz >= ($target_mzs->[$index] - ($target_mzs->[$index] * $mz_play_fraction)) && $mz <= ($target_mzs->[$index] + ($target_mzs->[$index] * $mz_play_fraction))]");
	if($mz >= ($target_mzs->[$index] -
		   ($target_mzs->[$index] * $mz_play_fraction)) &&
	   $mz <= ($target_mzs->[$index] +
		   ($target_mzs->[$index] * $mz_play_fraction)))
	  {push(@in_range,$target_mzs->[$index])}
	$index++;
      }

    my $closest = 0;
    if(scalar(@in_range))
      {($closest) = sort {abs($mz - $a) <=> abs($mz - $b)} @in_range}

    return($closest);
  }

#Significant digits
sub sigdig
  {
    my $num = $_[0];
    my $num_digits = $_[1];

    return($num) unless($num =~ /\d/);

    if($num =~ /e/i)
      {
	#Do nothing
      }
    elsif($num >= 10**($num_digits-1))
      {
	$num =~ s/\..*//;
	$num =~ s/(?<=\d{$num_digits})\d/0/g;
      }
    elsif($num =~ /^0*([1-9]\d*)\./)
      {
	my $len = length($1);
	$num_digits -= $len;
	$num =~ s/\.(\d{$num_digits})[^\.]*$/.$1/;
      }
    elsif($num =~ /\.(0*)[1-9]\d*/)
      {
	my $len = length($1);
	$num_digits += $len;
	$num =~ s/(?<=\d{$num_digits}).*//;
      }
    return($num);
  }

##
## This subroutine prints a description of the script and it's input and output
## files.
##
sub help
  {
    my $script = $0;
    my $lmd = localtime((stat($script))[9]);
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Print a description of this program
    print << "end_print";

$script
Copyright 2007
Robert W. Leach
Created on 8/14/2008
Last Modified on $lmd
Center for Computational Research
701 Ellicott Street
Buffalo, NY 14203
rwleach\@ccr.buffalo.edu

* WHAT IS THIS: This script takes a series of mass spectrum files (one for each
                sample) and a file of target MZ ratios and outputs the largest
                peak for each target and input file with assigned bins for
                comparison across samples.  The bins are numbered sequentially
                using one consistent sequence for all files.

* INPUT FORMAT: The input files are tab-delimited.  Comment lines (beginning
                with the # character are skipped as well as any line containing
                non-numeric characters).  Column order is peak center index,
                mass/charge value, peak value (intensity), signal to noise
                ratio, and peak scale (referring to the peak shape).  Example:

                  "peakCenterIndex"	"m.z"	"peakValue"	"peakSNR"	"peakScale"
                  16230	6821.0444	8541.581	31.336	68
                  17611	6989.0322	1982.070	7.271	68
                  27832	8295.8643	4876.837	17.891	68
                  30102	8601.291	1567.807	5.751	68
                  30921	8712.8418	84324.167	309.363	68

                Optionally, files containing only mass/charge value and peak
                value (intensity) are acceptable.  Example:

                  "m.z"	"peakValue"
                  6821.0444	8541.58178466885
                  6989.0322	1982.07055028410
                  8295.8643	4876.83717002771
                  8601.291	1567.80770272717
                  8712.8418	84324.1677819194

* INPUT MZ FORMAT: This is a file of target mass/charge (MZ) ratios.  The
                   largest peak in range of each target MZ is reported for each
                   input file.  The file of mass charge ratios contains an mz
                   value on each line like this:

                     6821.0444
                     6989.0322
                     8295.8643
                     8601.291
                     8712.8418

* OUTPUT FORMAT: The output format is the same as the input format except
                 comment/empty lines are not included and the peak center index
                 is replaced by an arbitrary/sequential bin index.  This index
                 will change based on the peak content of the files.  Example:

                   1	6821.0444	8541.581	31.336	68
                   2	6989.0322	1982.070	7.271	68
                   4	8295.8643	4876.837	17.891	68
                   6	8601.291	1567.807	5.751	68
                   7	8712.8418	84324.167	309.363	68

end_print

    return(0);
  }

##
## This subroutine prints a usage statement in long or short form depending on
## whether "no descriptions" is true.
##
sub usage
  {
    my $no_descriptions = $_[0];

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    print << "end_print";
USAGE: $script -i "input file(s)" [-o .ext] [-f] [-v] [-q] [-h] [--version] [--debug] < another_input_file
end_print

    if($no_descriptions)
      {print("Execute $script with no options to see a description of the ",
             "available parameters.\n")}
    else
      {
        print << 'end_print';

     -i|--input-file*     REQUIRED Space-separated peak file(s inside quotes)
                                   generated by Dongliang's R script.  See
                                   --help for a description of the input file
                                   format.
                                   *No flag required.  Standard input via
                                   redirection is acceptable.  Perl glob
                                   characters (e.g. '*') are acceptable inside
                                   quotes.
     -m|--mz-file         REQUIRED Space-separated file of target mass charge
                                   ratios.  See --help for a description of the
                                   input MZ file format.  Perl glob characters
                                   (e.g. '*') are acceptable inside quotes.
     -b|--bin-center-edge OPTIONAL [.0005] The fraction of the mass-charge
        -width-fraction            value chosen to be the bin center out from
                                   which to include peaks from other samples.
                                   A value of 0.05% is a fairly strict bin
                                   width determinant.  If you are getting
                                   warnings about peaks from the same spectra
                                   falling into the same bin, you can try to
                                   make this value smaller, but be careful that
                                   you are not creating excess bins containing
                                   peaks which should be in the same bin in
                                   the process.  Use the -t flag to throw out
                                   less intense peaks when multiple peaks fall
                                   into the same bin.
     -a|--use-amplitudes  OPTIONAL [Off] Use peak intensity to separate peaks
                                   in addition to the bin center-edge width
                                   fraction.  MassSpecWavelet will de-
                                   convolute the spectra such that a small
                                   narrow peak on top of a large broad peak
                                   will each be reported on.  If their centers
                                   fall into the same bin, then separating
                                   further based on intensity will put them in
                                   different bins.  This will only work on
                                   peaks from the same spectrum that fall into
                                   the same bin otherwise.  All samples should
                                   have both peaks present.  The separation
                                   fraction is not currently a parameter and is
                                   set at 0.2.  The peaks must differ in
                                   intensity by a 0.2 ratio to be put in
                                   different bins.
     -s|--use-scales      OPTIONAL [Off] Use peak scale to separate peaks in
                                   addition to the bin center-edge width
                                   fraction.  MassSpecWavelet will de-
                                   convolute the spectra such that a small
                                   narrow peak on top of a large broad peak
                                   will each be reported on.  If their centers
                                   fall into the same bin, then separating
                                   further based on scale will put them in
                                   different bins.  This will only work on
                                   peaks from the same spectrum that fall into
                                   the same bin otherwise.  All samples should
                                   have both peaks present.  The separation
                                   fraction is not currently a parameter and is
                                   set at 0.2.  The peaks must differ in scale
                                   by a 10 scales to be put in different bins.
     -n|--use-            OPTIONAL [Off] Use the signal-to-noise ratios to
        signal2noise               separate peaks in addition to the bin
                                   center-edge width fraction.
                                   MassSpecWavelet will de-convolutes the
                                   spectra such that a small narrow peak on top
                                   of a large broad peak will each be reported
                                   on.  If their centers fall into the same
                                   bin, then separating further based on
                                   signal to noise will put them in different
                                   bins.  This will only work on peaks from the
                                   same spectrum that fall into the same bin
                                   otherwise.  All samples should have both
                                   peaks present.  The separation fraction is
                                   not currently a parameter and is set at 0.5.
                                   The peaks must differ in signal to noise
                                   ratio values by a 0.2 ratio to be put in
                                   different bins.
     -o|--outfile-suffix  OPTIONAL [nothing] This suffix is added to the input
                                   file names to use as output files.
                                   Redirecting a file into this script will
                                   result in the output file name to be "STDIN"
                                   with your suffix appended.
     -f|--force           OPTIONAL [Off] Force overwrite of existing output
                                   files (generated from previous runs of this
                                   script).  Only used when the -o option is
                                   supplied.
     -v|--verbose         OPTIONAL [Off] Verbose mode.  Cannot be used with the
                                   quiet flag.
     -q|--quiet           OPTIONAL [Off] Quiet mode.  Turns off warnings and
                                   errors.  Cannot be used with the verbose
                                   flag.
     -h|--help            OPTIONAL [Off] Help.  Use this option to see an
                                   explanation of the script and its input and
                                   output files.
     --version            OPTIONAL [Off] Print software version number.  If
                                   verbose mode is on, it also prints the
                                   template version used to standard error.
     --debug              OPTIONAL [Off] Debug mode.

end_print
      }

    return(0);
  }


##
## Subroutine that prints formatted verbose messages.  Specifying a 1 as the
## first argument prints the message in overwrite mode (meaning subsequence
## verbose, error, warning, or debug messages will overwrite the message
## printed here.  However, specifying a hard return as the first character will
## override the status of the last line printed and keep it.  Global variables
## keep track of print length so that previous lines can be cleanly
## overwritten.
##
sub verbose
  {
    return(0) unless($verbose);

    #Read in the first argument and determine whether it's part of the message
    #or a value for the overwrite flag
    my $overwrite_flag = $_[0];

    #If a flag was supplied as the first parameter (indicated by a 0 or 1 and
    #more than 1 parameter sent in)
    if(scalar(@_) > 1 && ($overwrite_flag eq '0' || $overwrite_flag eq '1'))
      {shift(@_)}
    else
      {$overwrite_flag = 0}

    #Ignore the overwrite flag if STDOUT will be mixed in
    $overwrite_flag = 0 if(isStandardOutputToTerminal());

    #Read in the message
    my $verbose_message = join('',@_);

    $overwrite_flag = 1 if(!$overwrite_flag && $verbose_message =~ /\r/);

    #Initialize globals if not done already
    $main::last_verbose_size  = 0 if(!defined($main::last_verbose_size));
    $main::last_verbose_state = 0 if(!defined($main::last_verbose_state));
    $main::verbose_warning    = 0 if(!defined($main::verbose_warning));

    #Determine the message length
    my($verbose_length);
    if($overwrite_flag)
      {
	$verbose_message =~ s/\r$//;
	if(!$main::verbose_warning && $verbose_message =~ /\n|\t/)
	  {
	    warning("Hard returns and tabs cause overwrite mode to not work ",
		    "properly.");
	    $main::verbose_warning = 1;
	  }
      }
    else
      {chomp($verbose_message)}

    if(!$overwrite_flag)
      {$verbose_length = 0}
    elsif($verbose_message =~ /\n([^\n]*)$/)
      {$verbose_length = length($1)}
    else
      {$verbose_length = length($verbose_message)}

    #Overwrite the previous verbose message by appending spaces just before the
    #first hard return in the verbose message IF THE VERBOSE MESSAGE DOESN'T
    #BEGIN WITH A HARD RETURN.  However note that the length stored as the
    #last_verbose_size is the length of the last line printed in this message.
    if($verbose_message =~ /^([^\n]*)/ && $main::last_verbose_state &&
       $verbose_message !~ /^\n/)
      {
	my $append = ' ' x ($main::last_verbose_size - length($1));
	unless($verbose_message =~ s/\n/$append\n/)
	  {$verbose_message .= $append}
      }

    #If you don't want to overwrite the last verbose message in a series of
    #overwritten verbose messages, you can begin your verbose message with a
    #hard return.  This tells verbose() to not overwrite the last line that was
    #printed in overwrite mode.

    #Print the message to standard error
    print STDERR ($verbose_message,
		  ($overwrite_flag ? "\r" : "\n"));

    #Record the state
    $main::last_verbose_size  = $verbose_length;
    $main::last_verbose_state = $overwrite_flag;

    #Return success
    return(0);
  }

sub verboseOverMe
  {verbose(1,@_)}

##
## Subroutine that prints errors with a leading program identifier containing a
## trace route back to main to see where all the subroutine calls were from,
## the line number of each call, an error number, and the name of the script
## which generated the error (in case scripts are called via a system call).
##
sub error
  {
    return(0) if($quiet);

    #Gather and concatenate the error message and split on hard returns
    my @error_message = split("\n",join('',@_));
    pop(@error_message) if($error_message[-1] !~ /\S/);

    $main::error_number++;

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Assign the values from the calling subroutines/main
    my @caller_info = caller(0);
    my $line_num = $caller_info[2];
    my $caller_string = '';
    my $stack_level = 1;
    while(@caller_info = caller($stack_level))
      {
	my $calling_sub = $caller_info[3];
	$calling_sub =~ s/^.*?::(.+)$/$1/ if(defined($calling_sub));
	$calling_sub = (defined($calling_sub) ? $calling_sub : 'MAIN');
	$caller_string .= "$calling_sub(LINE$line_num):"
	  if(defined($line_num));
	$line_num = $caller_info[2];
	$stack_level++;
      }
    $caller_string .= "MAIN(LINE$line_num):";

    my $leader_string = "ERROR$main::error_number:$script:$caller_string ";

    #Figure out the length of the first line of the error
    my $error_length = length(($error_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $error_message[0]);

    #Put location information at the beginning of each line of the message
    foreach my $line (@error_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $error_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## Subroutine that prints warnings with a leader string containing a warning
## number
##
sub warning
  {
    return(0) if($quiet);

    $main::warning_number++;

    #Gather and concatenate the warning message and split on hard returns
    my @warning_message = split("\n",join('',@_));
    pop(@warning_message) if($warning_message[-1] !~ /\S/);

    my $leader_string = "WARNING$main::warning_number: ";

    #Figure out the length of the first line of the error
    my $warning_length = length(($warning_message[0] =~ /\S/ ?
				 $leader_string : '') .
				$warning_message[0]);

    #Put leader string at the beginning of each line of the message
    foreach my $line (@warning_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $warning_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## Subroutine that gets a line of input and accounts for carriage returns that
## many different platforms use instead of hard returns.  Note, it uses a
## global array reference variable ($infile_line_buffer) to keep track of
## buffered lines from multiple file handles.
##
sub getLine
  {
    my $file_handle = $_[0];

    #Set a global array variable if not already set
    $main::infile_line_buffer = {} if(!defined($main::infile_line_buffer));
    if(!exists($main::infile_line_buffer->{$file_handle}))
      {$main::infile_line_buffer->{$file_handle}->{FILE} = []}

    #If this sub was called in array context
    if(wantarray)
      {
	#Check to see if this file handle has anything remaining in its buffer
	#and if so return it with the rest
	if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) > 0)
	  {
	    return(@{$main::infile_line_buffer->{$file_handle}->{FILE}},
		   map
		   {
		     #If carriage returns were substituted and we haven't
		     #already issued a carriage return warning for this file
		     #handle
		     if(s/\r\n|\n\r|\r/\n/g &&
			!exists($main::infile_line_buffer->{$file_handle}
				->{WARNED}))
		       {
			 $main::infile_line_buffer->{$file_handle}->{WARNED}
			   = 1;
			 warning("Carriage returns were found in your file ",
				 "and replaced with hard returns");
		       }
		     split(/(?<=\n)/,$_);
		   } <$file_handle>);
	  }
	
	#Otherwise return everything else
	return(map
	       {
		 #If carriage returns were substituted and we haven't already
		 #issued a carriage return warning for this file handle
		 if(s/\r\n|\n\r|\r/\n/g &&
		    !exists($main::infile_line_buffer->{$file_handle}
			    ->{WARNED}))
		   {
		     $main::infile_line_buffer->{$file_handle}->{WARNED}
		       = 1;
		     warning("Carriage returns were found in your file ",
			     "and replaced with hard returns");
		   }
		 split(/(?<=\n)/,$_);
	       } <$file_handle>);
      }

    #If the file handle's buffer is empty, put more on
    if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) == 0)
      {
	my $line = <$file_handle>;
	if(!eof($file_handle))
	  {
	    if($line =~ s/\r\n|\n\r|\r/\n/g &&
	       !exists($main::infile_line_buffer->{$file_handle}->{WARNED}))
	      {
		$main::infile_line_buffer->{$file_handle}->{WARNED} = 1;
		warning("Carriage returns were found in your file and ",
			"replaced with hard returns");
	      }
	    @{$main::infile_line_buffer->{$file_handle}->{FILE}} =
	      split(/(?<=\n)/,$line);
	  }
	else
	  {
	    #Do the \r substitution for the last line of files that have the
	    #eof character at the end of the last line instead of on a line by
	    #itself.  I tested this on a file that was causing errors for the
	    #last line and it works.
	    $line =~ s/\r/\n/g if(defined($line));
	    @{$main::infile_line_buffer->{$file_handle}->{FILE}} = ($line);
	  }
      }

    #Shift off and return the first thing in the buffer for this file handle
    return($_ = shift(@{$main::infile_line_buffer->{$file_handle}->{FILE}}));
  }

##
## This subroutine allows the user to print debug messages containing the line
## of code where the debug print came from and a debug number.  Debug prints
## will only be printed (to STDERR) if the debug option is supplied on the
## command line.
##
sub debug
  {
    return(0) unless($DEBUG);

    $main::debug_number++;

    #Gather and concatenate the error message and split on hard returns
    my @debug_message = split("\n",join('',@_));
    pop(@debug_message) if($debug_message[-1] !~ /\S/);

    #Assign the values from the calling subroutine
    #but if called from main, assign the values from main
    my($junk1,$junk2,$line_num,$calling_sub);
    (($junk1,$junk2,$line_num,$calling_sub) = caller(1)) ||
      (($junk1,$junk2,$line_num) = caller());

    #Edit the calling subroutine string
    $calling_sub =~ s/^.*?::(.+)$/$1:/ if(defined($calling_sub));

    my $leader_string = "DEBUG$main::debug_number:LINE$line_num:" .
      (defined($calling_sub) ? $calling_sub : '') .
	' ';

    #Figure out the length of the first line of the error
    my $debug_length = length(($debug_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $debug_message[0]);

    #Put location information at the beginning of each line of the message
    foreach my $line (@debug_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $debug_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## This sub marks the time (which it pushes onto an array) and in scalar
## context returns the time since the last mark by default or supplied mark
## (optional) In array context, the time between all marks is always returned
## regardless of a supplied mark index
## A mark is not made if a mark index is supplied
## Uses a global time_marks array reference
##
sub markTime
  {
    #Record the time
    my $time = time();

    #Set a global array variable if not already set to contain (as the first
    #element) the time the program started (NOTE: "$^T" is a perl variable that
    #contains the start time of the script)
    $main::time_marks = [$^T] if(!defined($main::time_marks));

    #Read in the time mark index or set the default value
    my $mark_index = (defined($_[0]) ? $_[0] : -1);  #Optional Default: -1

    #Error check the time mark index sent in
    if($mark_index > (scalar(@$main::time_marks) - 1))
      {
	error("Supplied time mark index is larger than the size of the ",
	      "time_marks array.\nThe last mark will be set.");
	$mark_index = -1;
      }

    #Calculate the time since the time recorded at the time mark index
    my $time_since_mark = $time - $main::time_marks->[$mark_index];

    #Add the current time to the time marks array
    push(@$main::time_marks,$time)
      if(!defined($_[0]) || scalar(@$main::time_marks) == 0);

    #If called in array context, return time between all marks
    if(wantarray)
      {
	if(scalar(@$main::time_marks) > 1)
	  {return(map {$main::time_marks->[$_ - 1] - $main::time_marks->[$_]}
		  (1..(scalar(@$main::time_marks) - 1)))}
	else
	  {return(())}
      }

    #Return the time since the time recorded at the supplied time mark index
    return($time_since_mark);
  }

##
## This subroutine reconstructs the command entered on the command line
## (excluding standard input and output redirects).  The intended use for this
## subroutine is for when a user wants the output to contain the input command
## parameters in order to keep track of what parameters go with which output
## files.
##
sub getCommand
  {
    my $perl_path_flag = $_[0];
    my($command);

    #Determine the script name
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Put quotes around any parameters containing un-escaped spaces or astericks
    my $arguments = [@$preserve_args];
    foreach my $arg (@$arguments)
      {if($arg =~ /(?<!\\)[\s\*]/ || $arg eq '')
	 {$arg = "'" . $arg . "'"}}

    #Determine the perl path used (dependent on the `which` unix built-in)
    if($perl_path_flag)
      {
	$command = `which $^X`;
	chomp($command);
	$command .= ' ';
      }

    #Build the original command
    $command .= join(' ',($0,@$arguments));

    #Note, this sub doesn't add any redirected files in or out

    return($command);
  }

##
## This subroutine checks to see if a parameter is a single file with spaces in
## the name before doing a glob (which would break up the single file name
## improperly).  The purpose is to allow the user to enter a single input file
## name using double quotes and un-escaped spaces as is expected to work with
## many programs which accept individual files as opposed to sets of files.  If
## the user wants to enter multiple files, it is assumed that space delimiting
## will prompt the user to realize they need to escape the spaces in the file
## names.
##
sub sglob
  {
    my $command_line_string = $_[0];
    return(-e $command_line_string ?
	   $command_line_string : glob($command_line_string));
  }


sub printVersion
  {
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;
    print(($verbose ? "$script Version " : ''),
	  $software_version_number,
	  "\n");
    verbose("Generated using perl_script_template.pl\n",
	    "Version $template_version_number\n",
	    "Robert W. Leach\n",
	    "robleach\@lanl.gov\n",
	    "5/8/2006\n",
	    "Los Alamos National Laboratory\n",
	    "Copyright 2006");
    return(0);
  }

#This subroutine is a check to see if input is user-entered via a TTY (result is non-
#zero) or directed in (result is zero)
sub isStandardInputFromTerminal
  {return(-t STDIN || eof(STDIN))}

#This subroutine is a check to see if prints are going to a TTY.  Note, explicit prints
#to STDOUT when another output handle is selected are not considered and may defeat this
#subroutine.
sub isStandardOutputToTerminal
  {return(-t STDOUT && select() eq 'main::STDOUT')}
