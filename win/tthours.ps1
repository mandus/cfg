param ([string[]]$Args)
docker run -e XAL_CONSUMER=$env:XAL_CONSUMER -e XAL_EMPLOYEE=$env:XAL_EMPLOYEE --rm -it asmundodegard/tthours:latest $Args
