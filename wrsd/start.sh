#!/bin/bash
erl -setcookie wrsdpass -sname wrsdapp -pa ebin -yaws embedded true -boot start_sasl

