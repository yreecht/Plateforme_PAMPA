:: -*- coding: latin-1 -*-
:: Time-stamp: <2018-01-11 13:07:20 yreecht>

:: Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
::   Copyright (C) 2008-2017 Ifremer - Tous droits réservés.
::
::   Ce programme est un logiciel libre ; vous pouvez le redistribuer ou le
::   modifier suivant les termes de la "GNU General Public License" telle que
::   publiée par la Free Software Foundation : soit la version 2 de cette
::   licence, soit (à votre gré) toute version ultérieure.
::
::   Ce programme est distribué dans l'espoir qu'il vous sera utile, mais SANS
::   AUCUNE GARANTIE : sans même la garantie implicite de COMMERCIALISABILITÉ
::   ni d'ADÉQUATION À UN OBJECTIF PARTICULIER. Consultez la Licence Générale
::   Publique GNU pour plus de détails.
::
::   Vous devriez avoir reçu une copie de la Licence Générale Publique GNU avec
::   ce programme ; si ce n'est pas le cas, consultez :
::   <http://www.gnu.org/licenses/>.

@echo off

rem R version can be easily forced uncommenting/adapting the set command.
rem   For example, forcing R 2.15.3 (must be installed, loading fails otherwise):

rem set R_VER=R-2.15.3
set R_PROFILE=Scripts_Biodiv/Rprofile.site
cmd /c  .\Scripts_Biodiv\R.bat path ^& start "PAMPA WP2" Rgui.exe --no-restore --no-save --sdi
