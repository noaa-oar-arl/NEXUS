import requests
from datetime import datetime
from bs4 import BeautifulSoup
import os

class SessionWithHeaderRedirection(requests.Session):
    """NASA Session genergator

    Parameters
    ----------
    username : type
        Description of parameter `username`.
    password : type
        Description of parameter `password`.

    Attributes
    ----------
    auth : type
        Description of attribute `auth`.
    AUTH_HOST : type
        Description of attribute `AUTH_HOST`.

    """
    import requests
    AUTH_HOST = 'urs.earthdata.nasa.gov'

    def __init__(self, username, password):
        super().__init__()
        self.auth = (username, password)

    # Overrides from the library to keep headers when redirected to or from
    # the NASA auth host.
    def rebuild_auth(self, prepared_request, response):
        headers = prepared_request.headers
        url = prepared_request.url
        if 'Authorization' in headers:
            original_parsed = requests.utils.urlparse(response.request.url)
            redirect_parsed = requests.utils.urlparse(url)
            if (
                    original_parsed.hostname != redirect_parsed.hostname
            ) and redirect_parsed.hostname != self.AUTH_HOST and original_parsed.hostname != self.AUTH_HOST:
                del headers['Authorization']

        return


def get_nasa_data(username, password, filename):
    session = SessionWithHeaderRedirection(username, password)

    # the url of the file we wish to retrieve
    url = filename
    filename = filename.split(os.sep)[-1]
    try:
        # submit the request using the session
        response = session.get(url, stream=True)
#        print(response.status_code)
        # raise an exception in case of http errors
        response.raise_for_status()
        # save the file
        with open(filename, 'wb') as fd:
            for chunk in response.iter_content(chunk_size=1024 * 1024):
                fd.write(chunk)
    except requests.exceptions.HTTPError as e:
        # handle any errors here
        print(e)
        return None


def get_filenames_http(archive_url, ext):
    r = requests.get(archive_url)
    soup = BeautifulSoup(r.content, 'html.parser')
    links = soup.findAll('a')
    return [
        archive_url + link['href'] for link in links
        if link['href'].endswith('%s' % ext)
    ]


def get_nsidc_files_to_download(year,doy,user,passwd,ext='hdf',sat='MOST',product='MOD10A2.006',output_path='./'):
    import pandas as pd
    from numpy import array
    import requests
    d = pd.to_datetime('{}-{}'.format(year,doy),format='%Y-%j')
    baseurl = 'https://n5eil01u.ecs.nsidc.org'
    archive_url = '{}/{}/{}/{}/'.format(baseurl, sat, product,d.strftime('%Y.%m.%d'))
    with requests.Session() as session:
        r1 = session.request('get', archive_url)
        r = session.get(r1.url, auth=(user,passwd))
        if r.ok:
            content = r.content
    soup = BeautifulSoup(content, 'html.parser')
    links = soup.findAll('a')
    files = pd.Series([archive_url + link['href'] for link in links if link['href'].endswith('%s' % ext)]).drop_duplicates()
    basenames = [os.path.basename(f) for f in files]
    files_on_system = [
        os.path.isfile("{}/{}".format(output_path, f)) for f in basenames
    ]
    files_to_download = array(files)[~array(files_on_system)]
    return files_to_download, basenames

def get_modis_files_to_download(year,
                                doy,
                                tiles,
                                output_path,
                                ext,
                                sat='MOLA',
                                product='MYD09A1.006',archive='usgs'):
    from numpy import array
    import pandas as pd
    year = str(year)
    doy = str(doy).zfill(3)
    d = pd.to_datetime('{}-{}'.format(year,doy),format='%Y-%j')
    if archive == 'usgs':
        baseurl = 'https://e4ftl01.cr.usgs.gov'
    elif archive =='ladsweb':
        baseurl = 'https://ladsweb.modaps.eosdis.nasa.gov/archive/allData'
    else:
        baseurl = 'https://n5eil01u.ecs.nsidc.org'
    archive_url = '{}/{}/{}/{}/'.format(baseurl, sat, product,d.strftime('%Y.%m.%d'))
    print(archive_url)
    files = pd.Series(get_filenames_http(archive_url, ext)).drop_duplicates()    
    # GET THE FILES NOT CURRENTLY ON THE SYSTEM
    basenames = [os.path.basename(f) for f in files]
    files_on_system = [
        os.path.isfile("{}/{}".format(output_path, f)) for f in basenames
    ]
    files_to_download = array(files)[~array(files_on_system)]
    return files_to_download, basenames


def get_modis_available_days(dates, sat='MOLA', product='MYD09A1.006'):
    from numpy import array
    import pandas as pd
    baseurl = 'https://e4ftl01.cr.usgs.gov'
    archive_url = '{}/{}/{}/'.format(baseurl, sat, product)
    paths = get_filenames_http(archive_url, '/')
    datestr = [i.split('/')[-2].replace('.','') for i in paths[1:]]
    avail_dates = pd.to_datetime(datestr)
    overlap = pd.merge(pd.DataFrame(avail_dates),pd.DataFrame(dates),how='inner')
    dates = overlap[0]
    return dates


def create_netrc(user,passwd):
    import os
    if ~os.path.isfile(os.path.join(os.path.expanduser('~'),'.netrc')):
        f = open(os.path.join(os.path.expanduser('~'),'.netrc'),'w')
        f.write('machine urs.earthdata.nasa.gov login {} password {}'.format(user,passwd))
