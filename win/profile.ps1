Set-PSReadLineOption -EditMode vi -BellStyle None

# bash-style completions (possible options are MenuComplete or Complete, at least..)
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
Set-PSReadLineKeyHandler -Key Ctrl+n -Function AcceptNextSuggestionWord
Set-PSReadLineKeyHandler -Key Ctrl+f -Function AcceptSuggestion

oh-my-posh --init --shell pwsh --config c:\Users\57770\.ohmyposhv3-v2.json | Invoke-Expression

# fancy icons 
Import-Module -Name Terminal-Icons

# Use Fuzzy searcher
Import-Module -Name PSFzf
# Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t' -PSReadlineChordReverseHistory 'Ctrl+r'

# Alias
Set-Alias -Name g -Value (Get-Command git).Path

function gup {
	git up $args
}
function gpu {
	git push -f $args
}
function gst {
	git status -v -v
}

# Quickly visit directories
Import-Module z

# The "br" companion to broot doesn't install in powershell, but this function provide de functionality
function br {
    $tempFile = New-TemporaryFile
    try {
        $broot = $env:BROOT
        if (-not $broot) {
             $broot = 'broot'
        }
        & $broot --outcmd $tempFile $args
        if ($LASTEXITCODE -ne 0) {
            Write-Error "$broot exited with code $LASTEXITCODE"
            return
        }
        $command = Get-Content $tempFile
        if ($command) {
            # broot returns extended-length paths but not all PowerShell/Windows
            # versions might handle this so strip the '\\?'
            Invoke-Expression $command.Replace("\\?\", "")
        }
    } finally {
        Remove-Item -force $tempFile
    }
}

$isDotSourced = $MyInvocation.InvocationName -eq '.' -or $MyInvocation.Line -eq ''
if (-not $isDotSourced) {
    br $args
}

# add vim in PATH
$Env:PATH += ';C:\Program Files\Vim\vim91'

# add far in PATH
$Env:PATH += ';C:\Program Files\Far Manager\'


# setup for pyenv (pyenv needs to be first/early in PATH)
$env:PYENV = $HOME + "\.pyenv\pyenv-win"
$env:PYENV_ROOT = $env:PYENV
$env:PYENV_HOME = $env:PYENV
$env:PATH = $Env:PYENV + "\bin;" + $env:PYENV + "\shims;" + $env:PATH


# completion for az cli
Register-ArgumentCompleter -Native -CommandName az -ScriptBlock {
    param($commandName, $wordToComplete, $cursorPosition)
    $completion_file = New-TemporaryFile
    $env:ARGCOMPLETE_USE_TEMPFILES = 1
    $env:_ARGCOMPLETE_STDOUT_FILENAME = $completion_file
    $env:COMP_LINE = $wordToComplete
    $env:COMP_POINT = $cursorPosition
    $env:_ARGCOMPLETE = 1
    $env:_ARGCOMPLETE_SUPPRESS_SPACE = 0
    $env:_ARGCOMPLETE_IFS = "`n"
    $env:_ARGCOMPLETE_SHELL = 'powershell'
    az 2>&1 | Out-Null
    Get-Content $completion_file | Sort-Object | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, "ParameterValue", $_)
    }
    Remove-Item $completion_file, Env:\_ARGCOMPLETE_STDOUT_FILENAME, Env:\ARGCOMPLETE_USE_TEMPFILES, Env:\COMP_LINE, Env:\COMP_POINT, Env:\_ARGCOMPLETE, Env:\_ARGCOMPLETE_SUPPRESS_SPACE, Env:\_ARGCOMPLETE_IFS, Env:\_ARGCOMPLETE_SHELL
}

$env:XAL_CONSUMER = 
$env:XAL_EMPLOYEE = 
