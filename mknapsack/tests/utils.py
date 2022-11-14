"""Module for helper functions for testing."""


def get_id(params):
    name = ''
    for key in ['method', 'method_kwargs', 'case', 'verbose']:
        name += f'-{params.get(key, "NotSet")}'
    return name.strip('-')
