micro64disktool

===============


A c64 diskette image converter tool, which is based on extremly stripped micro64 source code.

#Usage

    Micro64 Disk Tool 20130113 - Copyright (C) 2012-2013, Benjamin 'BeRo' Rosseaux
    http://www.micro64.de/
    Usage: micro64disktool [options]
    Options: +newd64 [filename.d64]
             +g642d64 [input.g64] [output.d64]
             +d642g64 [input.d64] [output.g64]
             +g642p64 [input.g64] [output.p64]
             +p642g64 [input.p64] [output.g64]
             +fdi2p64 [input.fdi] [output.p64]
             +p642fdi [input.p64] [output.fdi]
             +nib2g64 [input.nib] [output.g64]
             +dumpfdi [input.fdi] [output.log]
             +dumpp64 [input.p64] [output.log]
             +dumpp64halftrack [input.p64] [output.log] ([halftrack])
             +kryofluxstream2p64 [inputpathprefix] [output.p64] ([side(0/1)]) ([rpm(180-360)])    ([doublewidetrack(0/1)])
             +kryofluxstream2fdi [inputpathprefix] [output.fdi] ([side(0/1)]) ([rpm(180-360)])    ([doublewidetrack(0/1)])

#License

    (*
    ** The source code of the Micro64 Disk Tool and helper tools are
    ** distributed under the Library GNU General Public License
    ** (see the file COPYING) with the following modification:
    **
    ** As a special exception, the copyright holders of this software give you
    ** permission to link this software with independent modules to produce 
    ** an executable, regardless of the license terms of these independent modules,
    ** and to copy and distribute the resulting executable under terms of your 
    ** choice, provided that you also meet, for each linked independent module,
    ** the terms and conditions of the license of that module. An independent 
    ** module is a module which is not derived from or based on this software. If 
    ** you modify this software, you may extend this exception to your  version of
    ** the software, but you are not obligated to do so. If you do not wish to do 
    ** so, delete this exception statement from your version.
    **
    ** If you didn't receive a copy of the file COPYING, contact:
    **      Free Software Foundation
    **      675 Mass Ave
    **      Cambridge, MA  02139
    **      USA
    **
    ** This program is distributed in the hope that it will be useful,
    ** but WITHOUT ANY WARRANTY; without even the implied warranty of
    ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    **
    *)

