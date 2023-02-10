from bs4 import BeautifulSoup
import copy
from dataclasses import dataclass
from flask import Flask, redirect, request
from lxml import etree, html
import os
import re
from typing import ClassVar

LOGGING = False
def log(*msg):
    if LOGGING:
        print(*msg)

app = Flask(__name__)

def pretty_print(tree, content_type):
    pretty = html.tostring(tree, method=content_type, encoding='unicode', pretty_print=True)
    bs = BeautifulSoup(pretty, content_type)
    pretty = bs.prettify()
    if content_type == 'xml':
        pretty = '\n'.join(pretty.split('\n')[1:])
    return pretty

def flatten(xs):
    xs2 = []
    for x in xs:
        if isinstance(x, list) and not etree.iselement(x):
            xs2 += flatten(x)
        else:
            xs2.append(x)
    return xs2

class Server:

    def __init__(self, root_dir):
        self.root_dir = root_dir

    RESERVED_FILE_NAMES = [
        'get.html',
        'get.xml',
        'post.html',
        'post.xml',
        'content.xml',
    ]
    def handle_request(self, method, content_type, request_path, request_content, request_params):
        # statically serve a file if it exists
        m = re.match('^/?([^/]+/)*([^/]+\.[^/]+)$', request_path)
        if m is not None:
            if m.group(2) in Server.RESERVED_FILE_NAMES:
                log(f'ERROR: CANNOT SERVE RESERVED FILE {request_path}')
                return None
            
            static_request_path = f'{self.root_dir}/{request_path}'
            if os.path.isfile(static_request_path):
                with open(static_request_path, 'r') as f:
                    return f.read()
        
        # statically serve index.html if it exists
        log('TESTING')
        m = re.match('^/?([^/]+(/[^/]+)*/?)?$', request_path)
        if m is not None:
            log('REQUEST PATH', request_path)
            if request_path == '':
                index_request_path = f'{self.root_dir}/index.html'
            else:
                index_request_path = f'{self.root_dir}/{request_path}/index.html'
            log('INDEX REQUEST PATH', index_request_path)
            if os.path.isfile(index_request_path):
                log('HAVE INDEX')
                with open(index_request_path, 'r') as f:
                    return f.read()
        log('TESTING 2')

        if content_type == 'both':
            content_type = 'html'
            res = server.resolve_path(f'{method}.html', server.root_dir, request_path.split('/'))
            if res is None:
                content_type = 'xml'
                res = server.resolve_path(f'{method}.xml', server.root_dir, request_path.split('/'))
        elif content_type in ['html', 'xml']:
            res = server.resolve_path(f'{method}.{content_type}', server.root_dir, request_path.split('/'))
        else:
            return None
        
        if res is None:
            return None

        handler_path, bindings = res
        log(f'Found handler path: {handler_path} with bindings: {bindings}')
        with open(handler_path, 'r') as f:
            handler = etree.fromstring(f.read())
        bindings['method'] = method
        bindings['content_type'] = content_type
        bindings['handler_path'] = handler_path[len(server.root_dir)+1:]
        bindings['request_path'] = request_path

        response_info = {}
        
        if method == 'get':
            log('GET PARAMS', request_params)
            if not isinstance(request_params, dict):
                request_params = {}
            
            bindings = { **bindings, **request_params }

            result = server.process_tree(method, response_info, bindings, handler)
            
            if result is None:
                return None
            
            if isinstance(result, list) and not etree.iselement(result):
                result = flatten(result)
                if len(result) == 0:
                    return None
                for x in result:
                    if etree.iselement(x):
                        result = x
                        break
            
            return pretty_print(result, content_type)
            
        elif method == 'post':
            log('POST CONTENT', request_content)
            if not isinstance(request_content, dict):
                request_content = {}
            new_id = self.new_id(request_path)
            new_dir_path = f'{self.root_dir}/{request_path}/{new_id}'
            target_path = f'{new_dir_path}/content.xml'
            os.makedirs(new_dir_path, exist_ok=True)
            bindings['new_id'] = new_id
            bindings['target_path'] = target_path
            bindings = { **bindings, **request_content }
            
            result = server.process_tree(method, response_info, bindings, handler)
            
            if result is None:
                return None
            
            if isinstance(result, list) and not etree.iselement(result):
                result = flatten(result)
                if len(result) == 0:
                    return None
                for x in result:
                    if etree.iselement(x):
                        result = x
                        break
            
            log(f'Writing to {target_path}:')
            
            if os.path.exists(target_path):
                return None
            
            with open(target_path, 'w') as f:
                f.write(pretty_print(result, content_type))
            log('OK POSTED')
            
            if 'redirect' in response_info:
                redirect_to = response_info['redirect']
            else:
                redirect_to = f'/{request_path}/{new_id}'
            return redirect(redirect_to)


    @staticmethod
    def valid_path(path):
        parts = path.split('/')
        return path[0] != '/' and '.' not in parts and '..' not in parts
    
    def new_id(self, path):
        dir_path = f'{self.root_dir}/{path}'
        os.makedirs(dir_path, exist_ok=True)
        items = [
            int(d)
            for d in os.listdir(dir_path)
            if os.path.isdir(f'{dir_path}/{d}') and re.match('^\d+$', d)
        ]
        log('DIRS:', os.listdir(dir_path))
        log('ITEMS:', items)

        if len(items) == 0:
            return 0
        else:
            return 1 + max(items)


    CAPTURE_SIGIL = '%'

    def resolve_path(self, file_name, dir_path, path):
        log(f'Trying {dir_path} =?= {path} for {file_name}')
        
        if len(path) == 0:
            # The path is empty and we don't have to look any further

            file_path = f'{dir_path}/{file_name}'
            if os.path.isfile(file_path):
                return (file_path, {})
            else:
                return None
        else:
            # The path is not empty, we need to recurse
            part = path[0]
            rest = path[1:]
            sub_path = f'{dir_path}/{part}'

            if os.path.isdir(sub_path):
                # There's a subdir with the appropriate name, look there
                rec = self.resolve_path(file_name, sub_path, rest)
                if rec:
                    # If we fine it, we're done
                    return rec
        
            # No subdir exists or it didn't have an appropriate file
            # Try finding a capture subdir 
            capture_subdir = [
                p
                for p in os.listdir(dir_path)
                if p[0] == Server.CAPTURE_SIGIL
            ]
            if capture_subdir:
                # Ther is a capture subdir, so try that
                subdir_name = capture_subdir[0]
                sub_path = f'{dir_path}/{subdir_name}'
                capture_name = subdir_name[1:]
                capture_value = part
                rec = self.resolve_path(file_name, sub_path, rest)
                if rec:
                    # We found a match
                    return (rec[0], { **rec[1], capture_name: capture_value })
                
            # No capture subdir exists, or it didn't match, so no match here
            log('No match at', dir_path, path)
            return None

    FOR_TAG = 'for'
    IF_TAG = 'if'
    INCLUDE_TAG = 'include'
    LET_TAG = 'let'
    RESULT_TAG = 'result'
    WITH_TAG = 'with'

    AUTHORIZED_ATTR = 'authorized'
    IN_ATTR = 'in'
    REDIRECT_ATTR = 'redirect'
    SORT_ATTR = 'sort'
    SRC_ATTR = 'src'
    TEST_ATTR = 'test'
    VAL_ATTR = 'val'
    VAR_ATTR = 'var'

    GET_TAGS = [
        FOR_TAG,
        INCLUDE_TAG,
        LET_TAG,
    ]
    POST_TAGS = [
        FOR_TAG,
        INCLUDE_TAG,
        LET_TAG,
        RESULT_TAG,
    ]
    TAGS = [
        *GET_TAGS,
        *POST_TAGS,
    ]
    def process_tree(self, method, response_info, bindings, tree):
        log()
        log('!!! PROCESSING', tree.tag, tree.attrib, repr(tree.text))
        
        if (method == 'get' and tree.tag not in Server.GET_TAGS and tree.tag in Server.TAGS) or\
            (method == 'post' and tree.tag not in Server.POST_TAGS and tree.tag in Server.TAGS):
            
            return None

        # new_attribs = self.process_attributes(method, tree, bindings)

        if tree.tag == Server.FOR_TAG:
            log(f'Found for: {tree}')
            return self.process_for(method, response_info, bindings, tree)

        elif tree.tag == Server.IF_TAG:
            log(f'Found if: {tree}')
            return self.process_if(method, response_info, bindings, tree)

        elif tree.tag == Server.INCLUDE_TAG:
            log(f'Found inclusion: {tree}')
            return self.process_include(method, response_info, bindings, tree)
        
        elif tree.tag == Server.LET_TAG:
            log(f'Found let binding: {tree}')
            return self.process_let(method, response_info, bindings, tree)
        
        elif tree.tag == Server.RESULT_TAG:
            log(f'Found response tag: {tree}')
            return self.process_response(method, response_info, bindings, tree)

        elif tree.tag == Server.WITH_TAG:
            log(f'Found with: {tree}')
            return self.process_with(method, response_info, bindings, tree)

        else:
            log(f'Found normal tree: {tree}')
            return self.process_normal(method, response_info, bindings, tree)
    
    NON_PROCESSED_ATTRIBUTES = [
        (FOR_TAG, VAL_ATTR),
        (FOR_TAG, VAR_ATTR),
        (IF_TAG, TEST_ATTR),
        (INCLUDE_TAG, VAL_ATTR),
        (LET_TAG, VAR_ATTR),
        (LET_TAG, VAL_ATTR),
        (WITH_TAG, VAL_ATTR),
    ]
    def process_attributes(self, method, bindings, tag, attribs):
        new_attribs = {}

        for attr in attribs:
            if (tag, attr) in Server.NON_PROCESSED_ATTRIBUTES:
                new_attribs[attr] = attribs[attr]
            else:
                unprocessed = attribs[attr]
                log(f'Processing attribute {attr}="{unprocessed}"')
                processed = self.substitute_attribute(unprocessed, bindings)
                if processed is not None:
                        new_attribs[attr] = processed
        
        return new_attribs
    
    def substitute_attribute(self, val, bindings):
        while True:
            m = re.search(f'{Server.CAPTURE_SIGIL}([a-zA-Z_]+)', val)
            if m is None:
                break
            found = m.group(1)
            replace = bindings.get(found)
            if replace is None:
                replace = ''
            val = val[:m.start()] + replace + val[m.end():]
            log('ATTR REPLACE:', repr(found), repr(replace), val)
        return val
    
    def process_children(self, method, response_info, bindings, children):
        new_children = []

        for child in children:
             c = self.process_tree(method, response_info, bindings, child)
             if c is not None:
                new_children.append(c)
        
        return new_children
    
    @staticmethod
    def set_attribs(tree, attribs):
        for attr in attribs:
            tree.set(attr, attribs[attr])
        
    @staticmethod
    def append_children(tree, children):
        for child in children:
            if isinstance(child, str) :
                if len(tree) == 0:
                    tree.text = (tree.text or '') + ' ' + child
                else:
                    tree[-1].tail = (tree[-1].tail or '') + ' ' + child
            elif isinstance(child, list):
                Server.append_children(tree, child)
            elif etree.iselement(child):
                tree.append(child)

    def process_normal(self, method, response_info, bindings, tree):
        new_tree = etree.Element(tree.tag)
        new_tree.text = tree.text
        new_tree.tail = tree.tail
        new_attribs = self.process_attributes(method, bindings, tree.tag, tree.attrib)
        Server.set_attribs(new_tree, new_attribs)
        new_children = self.process_children(method, response_info, bindings, list(tree))
        Server.append_children(new_tree, new_children)

        return new_tree
    
    def process_if(self, method, response_info, bindings, tree):
        attribs = self.process_attributes(method, bindings, tree.tag, tree.attrib)
        
        if Server.TEST_ATTR in attribs:
            test = AttrExpression.parse(attribs[Server.TEST_ATTR]).eval(bindings)
            
            if test:
                result = self.process_children(method, response_info, bindings, list(tree))
            else:
                result = []

            if tree.text:
                result.insert(0, tree.text)

            if tree.tail:
                result.append(tree.tail)
            
            return result
        
        elif Server.AUTHORIZED_ATTR in attribs:
            log('AUTHORIZATION_CONSTRAINT')
            return None
        
        else:
            return None
    
    def process_include(self, method, response_info, bindings, tree):
        attribs = self.process_attributes(method, bindings, tree.tag, tree.attrib)
        result = []
        
        if Server.SRC_ATTR in attribs:
            local_path = attribs[Server.SRC_ATTR]
            src = f'{self.root_dir}/{local_path}'
                
            if not local_path or not Server.valid_path(local_path) or not os.path.isfile(src):
                log(f'Not a valid path: {local_path}')
                return None
            
            with open(src, 'r') as f:
                inclusion = etree.fromstring(f.read())
            
            log(f'Replacing {html.tostring(tree)} with {html.tostring(inclusion)}')
            result.append(self.process_tree(method, response_info, bindings, inclusion))
            
        elif Server.VAL_ATTR in attribs:
            inclusion = AttrExpression.parse(attribs[Server.VAL_ATTR]).eval(bindings)
            
            if isinstance(inclusion, str) or\
                isinstance(inclusion, int) or\
                isinstance(inclusion, float) or\
                isinstance(inclusion, bool):

                inclusion = str(inclusion)
                log(f'Replacing {html.tostring(tree)} with string {repr(inclusion)}')
                result.append(inclusion)
            
            elif isinstance(inclusion, list):
                log(f'Replacing {html.tostring(tree)} with list {repr(inclusion)}')
                result.append(inclusion)
            
            elif etree.iselement(inclusion):
                log(f'Replacing {html.tostring(tree)} with element {html.tostring(inclusion)}')
                result.append(inclusion)
            
            else:
                log('WTFFFFFFFFFFFFFFFF', attribs[Server.VAL_ATTR], repr(inclusion))
                return None
                
        else:
            log(f'Nothing to replace {tree} with, so removing')
            return None
        
        if tree.text:
            result.insert(0, tree.text)

        if tree.tail:
            result.append(tree.tail)
        
        return result

    
    def process_let(self, method, response_info, bindings, tree):
        attribs = self.process_attributes(method, bindings, tree.tag, tree.attrib)
        if Server.VAR_ATTR not in attribs:
            return None
        var = attribs[Server.VAR_ATTR]


        if Server.VAL_ATTR in attribs:
            val = attribs[Server.VAL_ATTR]
            log(f'Let var = {var}, val (unnormalized) = {val}')
            val = AttrExpression.parse(val).eval(bindings)
            log(f'Let var = {var}, val (normalized) = {val}')

        elif Server.SRC_ATTR in attribs:
            local_path = attribs[Server.SRC_ATTR]
            src = f'{self.root_dir}/{local_path}'
                
            if not local_path or not Server.valid_path(local_path) or not os.path.isfile(src):
                log(f'Not a valid path: {local_path}')
                return None
            
            with open(src, 'r') as f:
                val = etree.fromstring(f.read())
        
        elif Server.IN_ATTR in attribs:
            src = f'{self.root_dir}/{attribs[Server.IN_ATTR]}'
        
            log('Found for:', var, src)
            if not os.path.isdir(src):
                log('Not a valid dirctory:', src)
                return None
            
            val = sorted([ x
                for x in os.listdir(src)
                if re.match('^\d+$', x)
            ])
        
        new_bindings = { **bindings, var: val }
        log(f'Let bindings = {new_bindings}')

        result = self.process_children(method, response_info, new_bindings, list(tree))

        if tree.text:
            result.insert(0, tree.text)

        if tree.tail:
            result.append(tree.tail)
        
        return result
        
    def process_for(self, method, response_info, bindings, tree):
        attribs = self.process_attributes(method, bindings, tree.tag, tree.attrib)
        
        if Server.VAR_ATTR not in attribs:
            return None
        var = attribs[Server.VAR_ATTR]
        
        if Server.IN_ATTR in attribs:
            src = f'{self.root_dir}/{attribs[Server.IN_ATTR]}'
        
            log('Found for:', var, src)
            if not os.path.isdir(src):
                log('Not a valid dirctory:', src)
                return None
            
            scrutinee = sorted([ x
                for x in os.listdir(src)
                if re.match('^\d+$', x)
            ])

        elif Server.VAL_ATTR in attribs:
            log('@@@ FOR VAL', attribs[Server.VAL_ATTR])
            scrutinee = AttrExpression.parse(attribs[Server.VAL_ATTR]).eval(bindings)
            log('@@@ FOR VAL', scrutinee)
            if etree.iselement(scrutinee):
                scrutinee = list(scrutinee)
            log('@@@ FOR VAL', scrutinee)

        else:
            scrutinee = []

        log('SSSSS', scrutinee)

        new_children = []
        for item in scrutinee:
            new_bindings = { **bindings, var: item }
            log('Foo item', item)
            log('For bindings', new_bindings)
            new_children += self.process_children(method, response_info, new_bindings, list(tree))
        
        result = new_children

        if tree.text:
            result.insert(0, tree.text)

        if tree.tail:
            result.append(tree.tail)
        
        return result
    
    def process_response(self, method, response_info, bindings, tree):
        attribs = self.process_attributes(method, bindings, tree.tag, tree.attrib)
        
        if Server.REDIRECT_ATTR in attribs:
            response_info['redirect'] = attribs[Server.REDIRECT_ATTR]
        
        result = self.process_children(method, response_info, bindings, list(tree))

        if tree.text:
            result.insert(0, tree.text)

        if tree.tail:
            result.append(tree.tail)
        
        return result


    
    def process_with(self, method, response_info, bindings, tree):
        attribs = self.process_attributes(method, bindings, tree.tag, tree.attrib)
        
        if Server.VAL_ATTR in attribs:
            scrutinee = AttrExpression.parse(attribs[Server.VAL_ATTR]).eval(bindings)
            
        elif Server.SRC_ATTR in attribs:
            local_path = attribs[Server.SRC_ATTR]
            src = f'{self.root_dir}/{local_path}'
                
            if not local_path or not Server.valid_path(local_path) or not os.path.isfile(src):
                log(f'Not a valid path: {local_path}')
                return None
            
            with open(src, 'r') as f:
                scrutinee = etree.fromstring(f.read())

        else:
            return None


        if scrutinee is None or not etree.iselement(scrutinee):
            return None
        
        with_bindings = {}
        for child in scrutinee:
            child_var = child.tag
            if child.text is None or child.text.strip() == '':
                with_bindings[child_var] = list(child)
            else:
                with_bindings[child_var] = child.text
        
        new_bindings = { **bindings, **with_bindings }
        log(f'With bindings = {new_bindings}')

        result = self.process_children(method, response_info, new_bindings, list(tree))
        
        if tree.text:
            result.insert(0, tree.text)

        if tree.tail:
            result.append(tree.tail)
        
        return result

class AttrExpression:
    @staticmethod
    def old_parse(s):
        if re.match(f'^{Server.CAPTURE_SIGIL}[a-zA-Z_]+$', s):
            return VarExpression(s[1:])
        else:
            return SymbolExpression(s)
        log('WTF', s)
    
    @staticmethod
    def parse(s):
        tokens = AttrExpression.lex(s)
        if tokens is None:
            return None
        tokens.reverse()
        
        stack = []
        while True:
            # try to reduce
            if AttrExpression.reduce(stack):
                continue

            # try to shift
            if AttrExpression.shift(stack, tokens):
                continue
                
            # try to finish
            if len(stack) == 1 and len(tokens) == 0:
                return stack[0]

            log('CANNOT PARSE', repr(s))
            log('FAILED WITH STACK', stack)
            log('FAILED WITH TOKENS', tokens)
            return None
    
    @staticmethod
    def reduce(stack):
        return AttrExpression.reduce_symbol(stack) or\
                AttrExpression.reduce_number(stack) or\
                AttrExpression.reduce_variable(stack) or\
                AttrExpression.reduce_binop(stack) or\
                AttrExpression.reduce_parens(stack)
                
    
    @staticmethod
    def reduce_symbol(stack):
        if len(stack) < 1 or not isinstance(stack[-1], SymbolToken):
            return False
        
        log('REDUCING SYMBOL', stack)
        stack.append(SymbolExpression(stack.pop().name))
        return True
    
    @staticmethod
    def reduce_number(stack):
        if len(stack) < 1 or not isinstance(stack[-1], NumberToken):
            return False
        
        log('REDUCING NUMBER', stack)
        stack.append(NumberExpression(stack.pop().value))
        return True
    
    @staticmethod
    def reduce_variable(stack):
        if len(stack) < 1 or not isinstance(stack[-1], VariableToken):
            return False
        
        log('REDUCING VARIABLE', stack)
        stack.append(VarExpression(stack.pop().name))
        return True

    @staticmethod
    def reduce_binop(stack):
        if len(stack) < 3 or\
            not isinstance(stack[-3], AttrExpression) or\
            not isinstance(stack[-2], OperatorToken) or\
            not isinstance(stack[-1], AttrExpression):

            return False
        
        log('REDUCING BINOP', stack)
        lexpr = stack[-3]
        op = stack[-2]
        rexpr = stack[-1]
        stack.pop()
        stack.pop()
        stack.pop()
        stack.append(BinopExpression(lexpr, op.name, rexpr))
        return True
    
    @staticmethod
    def reduce_parens(stack):
        if len(stack) < 3 or\
            not isinstance(stack[-3], LParenToken) or\
            not isinstance(stack[-2], AttrExpression) or\
            not isinstance(stack[-1], RParenToken):

            return False
        
        log('REDUCING PAREN', stack)
        expr = stack[-2]
        stack.pop()
        stack.pop()
        stack.pop()
        stack.append(expr)
        return True
    
    @staticmethod
    def shift(stack, tokens):
        if len(tokens) == 0:
            return False
        
        stack.append(tokens.pop())
        return True
    
    
    @staticmethod
    def lex(s):
        work_string = s.strip()
        tokens = []

        whitespace_pattern = '\s+'
        symbol_pattern = '[a-zA-Z_]+'
        number_pattern = '\d+(\.\d+)?'
        variable_pattern = f'{Server.CAPTURE_SIGIL}[a-zA-Z_]+(/[a-zA-Z_]+)*'
        # path_pattern = '(/[a-zA-Z_]+)+'
        lparen_pattern = '\('
        rparen_pattern = '\)'
        operator_pattern = '[~!@#\$%\^&\*\-=\+></\?]+'

        while work_string != '':
            log('PARSING', repr(work_string))
            # strip preceeding whitespace
            ws = re.match(whitespace_pattern, work_string)
            if ws is not None:
                work_string = work_string[len(ws.group(0)):]
            log('PARSING NO WHITE SPACE', repr(work_string))

            m = re.match(symbol_pattern, work_string)
            if m is not None:
                log('PARSER FOUND SYMBOL', m.group(0))
                sym = m.group(0)
                tokens.append(SymbolToken(sym))
                work_string = work_string[len(sym):]
                continue
            
            m = re.match(number_pattern, work_string)
            if m is not None:
                log('PARSER FOUND NUMBER', m.group(0))
                sym = m.group(0)
                tokens.append(NumberToken(sym))
                work_string = work_string[len(sym):]
                continue
            
            m = re.match(variable_pattern, work_string)
            if m is not None:
                log('PARSER FOUND VARIABLE', m.group(0))
                sym = m.group(0)
                tokens.append(VariableToken(sym[1:]))
                work_string = work_string[len(sym):]
                continue
                
            m = re.match(operator_pattern, work_string)
            if m is not None:
                log('PARSER FOUND OPERATOR', m.group(0))
                sym = m.group(0)
                tokens.append(OperatorToken(sym))
                work_string = work_string[len(sym):]
                continue
                
            m = re.match(lparen_pattern, work_string)
            if m is not None:
                log('PARSER FOUND LPAREN', m.group(0))
                sym = m.group(0)
                tokens.append(LParenToken())
                work_string = work_string[len(sym):]
                continue
            
            m = re.match(rparen_pattern, work_string)
            if m is not None:
                log('PARSER FOUND RPAREN', m.group(0))
                sym = m.group(0)
                tokens.append(RParenToken())
                work_string = work_string[len(sym):]
                continue
            
            
            log('PARSER FAILED ON ', repr(work_string))
            return None
        
        return tokens
    
    def __repr__(self):
        return f'{self.__class__.__name__}({",".join([ f"{f}={v}" for f,v in self.__dict__.items()])})'

class VarExpression(AttrExpression):
    def __init__(self, name):
        parts = name.split('/')
        self.name = parts[0]
        self.path = parts[1:]

    def eval(self, bindings):
        value = bindings.get(self.name)
        work_path = [*self.path]
        log('FINDMe', self, value, work_path)

        while True:
            if value is None:
                return None
            
            if len(work_path) == 0:
                if etree.iselement(value):
                    return None
                else:
                    return value
            
            if not etree.iselement(value):
                return None
            
            found = False
            for child in value:
                if child.tag == work_path[0]:
                    if len(child) == 0:
                        value = child.text
                    else:
                        value = child
                    work_path = work_path[1:]
                    found = True
                    break
                
            if not found:
                return None
        
        return None

@dataclass
class SymbolExpression(AttrExpression):
    name: str

    def eval(self, bindings):
        return self.name

class NumberExpression(AttrExpression):
    def __init__(self, value):
        try:
            self.value = int(value)
        except ValueError:
            self.value = float(value)
    
    def eval(self, bindings):
        return self.value

@dataclass
class BinopExpression(AttrExpression):
    OPERATORS: ClassVar[dict] = {
        '+': (lambda x, y: x+y),
        '-': (lambda x, y: x-y),
        '*': (lambda x, y: x*y),
        '/': (lambda x, y: x/y),
        '^': (lambda x, y: x**y),
        '%': (lambda x, y: x%y),
        '>': (lambda x, y: x>y),
        '>=': (lambda x, y: x>=y),
        '==': (lambda x, y: x==y),
        '!=': (lambda x, y: x!=y),
        '<=': (lambda x, y: x<=y),
        '<': (lambda x, y: x<y),
        '&&': (lambda x, y: x and y),
        '||': (lambda x, y: x or y),
    }
    left: AttrExpression
    operator: str
    right: AttrExpression

    def eval(self, bindings):
        if self.operator not in BinopExpression.OPERATORS:
            log('UNKNOWN OPERATOR', self)
            return None
        
        lval = self.left.eval(bindings)
        if lval is None:
            return None
        
        rval = self.right.eval(bindings)
        if rval is None:
            return None
        
        try:
            return BinopExpression.OPERATORS[self.operator](lval, rval)
        except ValueError:
            return None

class Token(object):
    pass

@dataclass
class SymbolToken(Token):
    name: str

@dataclass
class NumberToken(Token):
    value: str

@dataclass
class VariableToken(Token):
    name: str

@dataclass
class OperatorToken(Token):
    name: str

@dataclass
class LParenToken(Token):
    pass

@dataclass
class RParenToken(Token):
    pass

if __name__ == '__main__':
    LOGGING = True

    # expr = AttrExpression.new_parse('3 + (2 - %x/y)')
    # log(expr.eval({ 'x': 2 }))
 
    server = Server('./test')
    @app.route('/', defaults={'path': ''}, methods=['GET', 'POST'])
    @app.route('/<path:path>', methods=['GET', 'POST'])
    def main(path):
        # log('!!!!', dict(request.form))
        method = request.method.lower()

        if method == 'get':
            if 'text/html' in request.accept_mimetypes:
                content_type = 'html'
            elif 'application/xml' in request.accept_mimetypes:
                content_type = 'xml'
            elif 'application/xaml+xml' in request.accept_mimetypes:
                content_type = 'xml'
            else:
                content_type = 'both'
        elif method == 'post':
            content_type = 'xml'
        
        if request.form is None:
            content = {}
        else:
            content = dict(request.form)
        
        if request.args is None:
            params = {}
        else:
            params = dict(request.args)
        
        res = server.handle_request(method, content_type, path, content, params)
        if res is not None:
            return res
        
        return 'bad request', 400
    
    app.run()



    # print(server.handle_request('post', 'xml', 'quux/bar', ''))
