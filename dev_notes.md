# Development Notes

As this project was mainly developed as a learning exercise, I decided it would be a good idea to keep some notes about things I've learned and problems I've encountered along the way.

## Application and infinite call stack

This has certainly been the most persistent problem I've faced with writing the parser. I thought I had it solved, but it seems I need to revisit it...which has prompted me to make these notes.

When parsing a Term, we want to start by seeing if the Term can be an Application - there's no specific syntax that really indicates an Application apart from 2 sequential Terms, so we want to start by assuming it's an Application until we prove that it isn't. This presents a problem from a Parsing perspective, because the parser ends up in this infinite loop of `Application(Application(Application(...` without actually consuming any characters.

I thought I had this solved, by requiring the first term to be something concrete (ie. not Application), I avoid the infinite Application loop, and my parser started working. Then I started work on performing β-reduction on the parsed term, and realised I have a new problem.

Starting with the term `(λxyz.zxy)ab` as a simple test for β-reduction, I realised that the parsed result isn't really representing things properly. Ideally, we'd have `a` being applied to `(λxyz.zxy)` and then `b` being applied next, but instead, because of the way I solved the infinite loop problem, I have `b` being applied to `a` and then that Application being applied to `λxyz.zxy`. To get it to look the way I want, I need to find a way for Application to allow Application as a first term, but avoid the infinite loop.
